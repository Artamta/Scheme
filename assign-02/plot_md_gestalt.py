#!/usr/bin/env python3
"""
Create clean, easy-to-read MD plots from CSV trajectories.

Extra feature:
- Optional simulation animation export (GIF) per input CSV.
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Dict, Iterable, List, Sequence, Tuple

try:
    import matplotlib.pyplot as plt
    from matplotlib.animation import FuncAnimation, PillowWriter
    from matplotlib.lines import Line2D
except ImportError as exc:
    raise SystemExit(
        "matplotlib is required. Install it with: pip install matplotlib"
    ) from exc


CANVAS = "#f8fafc"
AX_FACE = "#ffffff"
GRID = "#dbe3ef"
TEXT = "#1f2937"

SPECIES_COLORS = {
    "A": "#2563eb",
    "B": "#ef4444",
    "C": "#059669",
}

FALLBACK_COLORS = ["#8b5cf6", "#f59e0b", "#14b8a6", "#ec4899", "#64748b"]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create clean MD plots and optional simulation GIFs from CSV files."
    )
    parser.add_argument(
        "--input",
        nargs="+",
        required=True,
        help="Input CSV file(s), e.g. output/two_species_2d.csv",
    )
    parser.add_argument(
        "--outdir",
        default="plots",
        help="Directory for static plot images (default: plots)",
    )
    parser.add_argument(
        "--dpi",
        type=int,
        default=260,
        help="Image DPI for static plots (default: 260)",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Show static plot windows interactively",
    )
    parser.add_argument(
        "--simulate",
        action="store_true",
        help="Generate animation GIF simulation per CSV",
    )
    parser.add_argument(
        "--sim-outdir",
        default="plots",
        help="Directory for simulation GIF outputs (default: plots)",
    )
    parser.add_argument(
        "--fps",
        type=int,
        default=10,
        help="Animation frames per second (default: 10)",
    )
    parser.add_argument(
        "--trail-length",
        type=int,
        default=18,
        help="Number of recent points shown as trajectory trail in animation (default: 18)",
    )
    return parser.parse_args()


def load_csv_rows(path: Path) -> List[dict]:
    with path.open("r", newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def find_component_columns(header: Sequence[str], prefix: str) -> List[str]:
    cols = [col for col in header if col.startswith(prefix)]
    cols.sort(key=lambda name: int(name[len(prefix) :]))
    return cols


def species_color(species: str) -> str:
    if species in SPECIES_COLORS:
        return SPECIES_COLORS[species]
    return FALLBACK_COLORS[abs(hash(species)) % len(FALLBACK_COLORS)]


def to_float_list(row: dict, keys: Iterable[str]) -> List[float]:
    return [float(row[k]) for k in keys]


def magnitude(vec: Sequence[float]) -> float:
    return sum(v * v for v in vec) ** 0.5


def group_by_particle(rows: Sequence[dict]) -> Dict[int, List[dict]]:
    grouped: Dict[int, List[dict]] = {}
    for row in rows:
        pid = int(row["particle"])
        grouped.setdefault(pid, []).append(row)
    for pid in grouped:
        grouped[pid].sort(key=lambda item: int(item["step"]))
    return grouped


def setup_matplotlib_theme() -> None:
    plt.rcParams.update(
        {
            "figure.facecolor": CANVAS,
            "axes.facecolor": AX_FACE,
            "axes.edgecolor": "#a5b4c8",
            "axes.labelcolor": TEXT,
            "axes.titlecolor": TEXT,
            "text.color": TEXT,
            "xtick.color": TEXT,
            "ytick.color": TEXT,
            "grid.color": GRID,
            "grid.alpha": 0.6,
            "grid.linewidth": 0.9,
            "font.family": "DejaVu Sans",
            "axes.titleweight": "semibold",
        }
    )


def style_axis(ax) -> None:
    ax.grid(True)
    for spine in ax.spines.values():
        spine.set_linewidth(0.9)
        spine.set_alpha(0.9)


def dimension_name(dim: int) -> str:
    return {1: "1D", 2: "2D", 3: "3D"}.get(dim, f"{dim}D")


def axis_limits(values: List[float], margin_ratio: float = 0.08) -> Tuple[float, float]:
    low = min(values)
    high = max(values)
    if low == high:
        pad = 1.0
        return low - pad, high + pad
    pad = (high - low) * margin_ratio
    return low - pad, high + pad


def linear_space(start: float, end: float, count: int) -> List[float]:
    if count <= 1:
        return [start]
    return [start + (end - start) * i / (count - 1) for i in range(count)]


def gradient_alphas(count: int, start_alpha: float = 0.2, end_alpha: float = 0.98) -> List[float]:
    if count <= 1:
        return [end_alpha]
    return [start_alpha + (end_alpha - start_alpha) * i / (count - 1) for i in range(count)]


def sampled_indices(length: int, max_points: int = 14) -> List[int]:
    if length <= 0:
        return []
    stride = max(1, length // max_points)
    indices = list(range(0, length, stride))
    if indices[-1] != length - 1:
        indices.append(length - 1)
    return indices


def draw_gradient_path_2d(ax, x: List[float], y: List[float], color: str, linewidth: float = 2.1) -> None:
    segment_count = max(0, len(x) - 1)
    for idx, alpha in enumerate(gradient_alphas(segment_count)):
        ax.plot(x[idx : idx + 2], y[idx : idx + 2], color=color, linewidth=linewidth, alpha=alpha, zorder=3)


def draw_gradient_path_3d(ax, x: List[float], y: List[float], z: List[float], color: str, linewidth: float = 1.9) -> None:
    segment_count = max(0, len(x) - 1)
    for idx, alpha in enumerate(gradient_alphas(segment_count)):
        ax.plot(
            x[idx : idx + 2],
            y[idx : idx + 2],
            z[idx : idx + 2],
            color=color,
            linewidth=linewidth,
            alpha=alpha,
            zorder=3,
        )


def draw_history_dots_2d(ax, x: List[float], y: List[float], color: str) -> None:
    indices = sampled_indices(len(x), max_points=16)
    if not indices:
        return
    alphas = gradient_alphas(len(indices), start_alpha=0.18, end_alpha=0.85)
    sizes = linear_space(10.0, 26.0, len(indices))
    for i, alpha, size in zip(indices, alphas, sizes):
        ax.scatter(x[i], y[i], color=color, alpha=float(alpha), s=float(size), zorder=4)


def draw_history_dots_3d(ax, x: List[float], y: List[float], z: List[float], color: str) -> None:
    indices = sampled_indices(len(x), max_points=16)
    if not indices:
        return
    alphas = gradient_alphas(len(indices), start_alpha=0.18, end_alpha=0.85)
    sizes = linear_space(8.0, 20.0, len(indices))
    for i, alpha, size in zip(indices, alphas, sizes):
        ax.scatter(x[i], y[i], z[i], color=color, alpha=float(alpha), s=float(size), zorder=4)


def add_soft_gradient_background_2d(ax, x_lim: Tuple[float, float], y_lim: Tuple[float, float]) -> None:
    grid_size = 180
    diagonal = []
    for j in range(grid_size):
        y = j / (grid_size - 1)
        row = []
        for i in range(grid_size):
            x = i / (grid_size - 1)
            row.append((x + y) * 0.5)
        diagonal.append(row)
    ax.imshow(
        diagonal,
        origin="lower",
        extent=[x_lim[0], x_lim[1], y_lim[0], y_lim[1]],
        cmap="Blues",
        alpha=0.11,
        interpolation="bilinear",
        aspect="auto",
        zorder=0,
    )


def extract_plot_data(csv_path: Path):
    rows = load_csv_rows(csv_path)
    if not rows:
        raise ValueError(f"No rows found in {csv_path}")

    header = list(rows[0].keys())
    x_cols = find_component_columns(header, "x")
    v_cols = find_component_columns(header, "v")
    f_cols = find_component_columns(header, "f")
    dim = len(x_cols)
    if dim not in (1, 2, 3):
        raise ValueError(f"Unsupported dimension: {dim}")

    grouped = group_by_particle(rows)
    return rows, grouped, x_cols, v_cols, f_cols, dim


def plot_file(csv_path: Path, outdir: Path, dpi: int, show: bool) -> Path:
    _, grouped, x_cols, v_cols, f_cols, dim = extract_plot_data(csv_path)

    all_positions = [to_float_list(row, x_cols) for entries in grouped.values() for row in entries]
    all_steps = [int(row["step"]) for entries in grouped.values() for row in entries]
    all_x = [p[0] for p in all_positions]
    all_y = [p[1] for p in all_positions] if dim >= 2 else []
    all_z = [p[2] for p in all_positions] if dim == 3 else []

    fig = plt.figure(figsize=(14, 8), facecolor=CANVAS)
    grid = fig.add_gridspec(2, 2, height_ratios=[2.0, 1.0], hspace=0.34, wspace=0.23)

    if dim == 3:
        ax_traj = fig.add_subplot(grid[0, :], projection="3d")
    else:
        ax_traj = fig.add_subplot(grid[0, :])
    ax_speed = fig.add_subplot(grid[1, 0])
    ax_force = fig.add_subplot(grid[1, 1])

    if dim == 1:
        x_lim = axis_limits([float(s) for s in all_steps], margin_ratio=0.03)
        y_lim = axis_limits(all_x)
        ax_traj.set_xlim(x_lim[0], x_lim[1])
        ax_traj.set_ylim(y_lim[0], y_lim[1])
        add_soft_gradient_background_2d(ax_traj, x_lim, y_lim)
    elif dim == 2:
        x_lim = axis_limits(all_x)
        y_lim = axis_limits(all_y)
        ax_traj.set_xlim(x_lim[0], x_lim[1])
        ax_traj.set_ylim(y_lim[0], y_lim[1])
        add_soft_gradient_background_2d(ax_traj, x_lim, y_lim)
    else:
        ax_traj.set_xlim(*axis_limits(all_x))
        ax_traj.set_ylim(*axis_limits(all_y))
        ax_traj.set_zlim(*axis_limits(all_z))

    legend_handles = []
    legend_labels = []

    for pid in sorted(grouped.keys()):
        entries = grouped[pid]
        species = entries[0]["species"]
        color = species_color(species)
        label = f"P{pid} ({species})"

        steps = [int(row["step"]) for row in entries]
        pos = [to_float_list(row, x_cols) for row in entries]
        vel = [to_float_list(row, v_cols) for row in entries]
        frc = [to_float_list(row, f_cols) for row in entries]

        speed_series = [magnitude(v) for v in vel]
        force_series = [magnitude(f) for f in frc]

        if dim == 1:
            y = [p[0] for p in pos]
            draw_gradient_path_2d(ax_traj, [float(s) for s in steps], y, color, linewidth=2.1)
            draw_history_dots_2d(ax_traj, [float(s) for s in steps], y, color)
            ax_traj.scatter(steps[0], y[0], color=color, s=40, marker="o", zorder=5)
            ax_traj.scatter(steps[-1], y[-1], color=color, s=52, marker="X", zorder=6)
            (line,) = ax_traj.plot([], [], color=color, linewidth=2.1, alpha=0.95)
            ax_traj.set_xlabel("Step")
            ax_traj.set_ylabel("Position x1")
        elif dim == 2:
            x = [p[0] for p in pos]
            y = [p[1] for p in pos]
            draw_gradient_path_2d(ax_traj, x, y, color, linewidth=2.1)
            draw_history_dots_2d(ax_traj, x, y, color)
            ax_traj.scatter(x[0], y[0], color=color, s=40, marker="o", zorder=5)
            ax_traj.scatter(x[-1], y[-1], color=color, s=52, marker="X", zorder=6)
            (line,) = ax_traj.plot([], [], color=color, linewidth=2.1, alpha=0.95)
            ax_traj.set_xlabel("x1")
            ax_traj.set_ylabel("x2")
        else:
            x = [p[0] for p in pos]
            y = [p[1] for p in pos]
            z = [p[2] for p in pos]
            draw_gradient_path_3d(ax_traj, x, y, z, color, linewidth=1.9)
            draw_history_dots_3d(ax_traj, x, y, z, color)
            ax_traj.scatter(x[0], y[0], z[0], color=color, s=30, marker="o", zorder=5)
            ax_traj.scatter(x[-1], y[-1], z[-1], color=color, s=44, marker="X", zorder=6)
            (line,) = ax_traj.plot([], [], [], color=color, linewidth=1.9, alpha=0.95)
            ax_traj.set_xlabel("x1")
            ax_traj.set_ylabel("x2")
            ax_traj.set_zlabel("x3")
            ax_traj.xaxis.pane.set_facecolor((0.80, 0.88, 0.99, 0.35))
            ax_traj.yaxis.pane.set_facecolor((0.86, 0.93, 0.99, 0.32))
            ax_traj.zaxis.pane.set_facecolor((0.93, 0.97, 1.00, 0.30))
            ax_traj.grid(True)

        ax_speed.plot(steps, speed_series, color=color, linewidth=1.9, alpha=0.95)
        ax_force.plot(steps, force_series, color=color, linewidth=1.9, alpha=0.95)

        legend_handles.append(line)
        legend_labels.append(label)

    if dim in (1, 2):
        style_axis(ax_traj)
    style_axis(ax_speed)
    style_axis(ax_force)

    ax_traj.set_title(f"{dimension_name(dim)} Particle Trajectories - {csv_path.name}", fontsize=14)
    ax_speed.set_title("Speed Magnitude |v|", fontsize=12)
    ax_force.set_title("Force Magnitude |F|", fontsize=12)
    ax_speed.set_xlabel("Step")
    ax_speed.set_ylabel("|v|")
    ax_force.set_xlabel("Step")
    ax_force.set_ylabel("|F|")

    legend = ax_traj.legend(
        legend_handles,
        legend_labels,
        loc="upper left",
        frameon=True,
        framealpha=0.95,
        fontsize=9,
        ncol=2,
    )
    legend.get_frame().set_facecolor("#ffffff")
    legend.get_frame().set_edgecolor("#c7d2e0")

    fig.subplots_adjust(left=0.06, right=0.98, bottom=0.08, top=0.93, wspace=0.23, hspace=0.34)

    outdir.mkdir(parents=True, exist_ok=True)
    output_path = outdir / f"{csv_path.stem}_clean.png"
    fig.savefig(output_path, dpi=dpi, bbox_inches="tight")
    if show:
        plt.show()
    plt.close(fig)
    return output_path


def select_closing_pair(steps: List[int], tracks: Dict[int, List[List[float]]]) -> Tuple[int, int, List[float]]:
    pids = sorted(tracks.keys())
    if len(pids) < 2:
        raise ValueError("Need at least 2 particles for a closer-distance plot.")

    best_gain = float("-inf")
    best_pair = (pids[0], pids[1])
    best_distances: List[float] = []

    for i in range(len(pids)):
        for j in range(i + 1, len(pids)):
            pa = pids[i]
            pb = pids[j]
            distances = [abs(tracks[pa][k][0] - tracks[pb][k][0]) for k in range(len(steps))]
            gain = distances[0] - min(distances)
            if gain > best_gain:
                best_gain = gain
                best_pair = (pa, pb)
                best_distances = distances

    return best_pair[0], best_pair[1], best_distances


def plot_1d_closer_distance(csv_path: Path, outdir: Path, dpi: int, show: bool) -> Path | None:
    _, grouped, x_cols, _, _, dim = extract_plot_data(csv_path)
    if dim != 1 or len(grouped) < 2:
        return None

    steps, tracks, species_by_pid = build_tracks(grouped, x_cols)
    pid_a, pid_b, distances = select_closing_pair(steps, tracks)

    pos_a = [p[0] for p in tracks[pid_a]]
    pos_b = [p[0] for p in tracks[pid_b]]

    color_a = species_color(species_by_pid[pid_a])
    color_b = species_color(species_by_pid[pid_b])

    fig = plt.figure(figsize=(12, 7.6), facecolor=CANVAS)
    grid = fig.add_gridspec(2, 1, height_ratios=[2.0, 1.2], hspace=0.28)
    ax_pos = fig.add_subplot(grid[0, 0])
    ax_dist = fig.add_subplot(grid[1, 0])

    x_lim = axis_limits([float(s) for s in steps], margin_ratio=0.03)
    y_lim = axis_limits(pos_a + pos_b)
    ax_pos.set_xlim(x_lim[0], x_lim[1])
    ax_pos.set_ylim(y_lim[0], y_lim[1])
    add_soft_gradient_background_2d(ax_pos, x_lim, y_lim)

    draw_gradient_path_2d(ax_pos, [float(s) for s in steps], pos_a, color_a, linewidth=2.3)
    draw_gradient_path_2d(ax_pos, [float(s) for s in steps], pos_b, color_b, linewidth=2.3)
    draw_history_dots_2d(ax_pos, [float(s) for s in steps], pos_a, color_a)
    draw_history_dots_2d(ax_pos, [float(s) for s in steps], pos_b, color_b)

    ax_pos.scatter(steps[0], pos_a[0], color=color_a, s=46, marker="o", zorder=5)
    ax_pos.scatter(steps[-1], pos_a[-1], color=color_a, s=58, marker="X", zorder=6)
    ax_pos.scatter(steps[0], pos_b[0], color=color_b, s=46, marker="o", zorder=5)
    ax_pos.scatter(steps[-1], pos_b[-1], color=color_b, s=58, marker="X", zorder=6)

    style_axis(ax_pos)
    ax_pos.set_title(f"1D Particles Moving Closer - {csv_path.name}", fontsize=14)
    ax_pos.set_xlabel("Step")
    ax_pos.set_ylabel("Position x1")
    ax_pos.legend(
        [
            Line2D([0], [0], color=color_a, linewidth=2.3),
            Line2D([0], [0], color=color_b, linewidth=2.3),
        ],
        [f"P{pid_a} ({species_by_pid[pid_a]})", f"P{pid_b} ({species_by_pid[pid_b]})"],
        loc="upper left",
        frameon=True,
        framealpha=0.95,
    )

    ax_dist.plot(steps, distances, color="#0f766e", linewidth=2.4, alpha=0.98)
    ax_dist.fill_between(steps, distances, color="#99f6e4", alpha=0.36)
    min_idx = min(range(len(distances)), key=lambda idx: distances[idx])
    ax_dist.scatter(steps[min_idx], distances[min_idx], color="#0f766e", marker="D", s=52, zorder=5)
    ax_dist.annotate(
        f"closest = {distances[min_idx]:.3f}",
        xy=(steps[min_idx], distances[min_idx]),
        xytext=(10, 10),
        textcoords="offset points",
        fontsize=10,
        color="#115e59",
    )

    style_axis(ax_dist)
    ax_dist.set_title("Distance Between Selected Pair", fontsize=12)
    ax_dist.set_xlabel("Step")
    ax_dist.set_ylabel("|xA - xB|")

    outdir.mkdir(parents=True, exist_ok=True)
    output_path = outdir / f"{csv_path.stem}_1d_closer.png"
    fig.savefig(output_path, dpi=dpi, bbox_inches="tight")
    if show:
        plt.show()
    plt.close(fig)
    return output_path


def build_tracks(grouped: Dict[int, List[dict]], x_cols: List[str]):
    steps = sorted({int(row["step"]) for rows in grouped.values() for row in rows})
    tracks: Dict[int, List[List[float]]] = {}
    species_by_pid: Dict[int, str] = {}

    for pid in sorted(grouped.keys()):
        entries = grouped[pid]
        species_by_pid[pid] = entries[0]["species"]
        by_step = {int(row["step"]): to_float_list(row, x_cols) for row in entries}
        last = by_step[min(by_step.keys())]
        sequence = []
        for step in steps:
            if step in by_step:
                last = by_step[step]
            sequence.append(last)
        tracks[pid] = sequence

    return steps, tracks, species_by_pid


def create_simulation(csv_path: Path, sim_outdir: Path, fps: int, trail_length: int) -> Path:
    _, grouped, x_cols, _, _, dim = extract_plot_data(csv_path)
    steps, tracks, species_by_pid = build_tracks(grouped, x_cols)
    pids = sorted(tracks.keys())

    if dim == 3:
        fig = plt.figure(figsize=(8.8, 7.2), facecolor=CANVAS)
        ax = fig.add_subplot(111, projection="3d")
    else:
        fig, ax = plt.subplots(figsize=(8.8, 6.8), facecolor=CANVAS)

    all_x = []
    all_y = []
    all_z = []
    for pid in pids:
        for p in tracks[pid]:
            all_x.append(p[0])
            if dim >= 2:
                all_y.append(p[1])
            if dim == 3:
                all_z.append(p[2])

    if dim == 1:
        x_lim = axis_limits([float(s) for s in steps], margin_ratio=0.03)
        y_lim = axis_limits(all_x)
        ax.set_xlim(x_lim[0], x_lim[1])
        ax.set_ylim(y_lim[0], y_lim[1])
        add_soft_gradient_background_2d(ax, x_lim, y_lim)
        ax.set_xlabel("Step")
        ax.set_ylabel("Position x1")
    elif dim == 2:
        x_lim = axis_limits(all_x)
        y_lim = axis_limits(all_y)
        ax.set_xlim(x_lim[0], x_lim[1])
        ax.set_ylim(y_lim[0], y_lim[1])
        add_soft_gradient_background_2d(ax, x_lim, y_lim)
        ax.set_xlabel("x1")
        ax.set_ylabel("x2")
    else:
        x_lim = axis_limits(all_x)
        y_lim = axis_limits(all_y)
        z_lim = axis_limits(all_z)
        ax.set_xlim(x_lim[0], x_lim[1])
        ax.set_ylim(y_lim[0], y_lim[1])
        ax.set_zlim(z_lim[0], z_lim[1])
        ax.set_xlabel("x1")
        ax.set_ylabel("x2")
        ax.set_zlabel("x3")
        ax.xaxis.pane.set_facecolor((0.80, 0.88, 0.99, 0.35))
        ax.yaxis.pane.set_facecolor((0.86, 0.93, 0.99, 0.32))
        ax.zaxis.pane.set_facecolor((0.93, 0.97, 1.00, 0.30))

    if dim in (1, 2):
        style_axis(ax)
    else:
        ax.grid(True)

    ax.set_title(f"{dimension_name(dim)} Simulation - {csv_path.name}", fontsize=13)

    trails = {}
    dots = {}
    visited = {}
    for pid in pids:
        color = species_color(species_by_pid[pid])
        if dim == 3:
            (trail,) = ax.plot([], [], [], color=color, linewidth=2.0, alpha=0.95)
            (dot,) = ax.plot([], [], [], marker="o", color=color, markersize=6)
            history = ax.scatter([], [], [], color=color, s=10, alpha=0.23, depthshade=False)
        else:
            (trail,) = ax.plot([], [], color=color, linewidth=2.0, alpha=0.95)
            (dot,) = ax.plot([], [], marker="o", color=color, markersize=6)
            history = ax.scatter([], [], color=color, s=10, alpha=0.23)
        trails[pid] = trail
        dots[pid] = dot
        visited[pid] = history

    if dim == 3:
        step_text = ax.text2D(0.02, 0.96, "", transform=ax.transAxes, color=TEXT, fontsize=10)
    else:
        step_text = ax.text(0.02, 0.96, "", transform=ax.transAxes, color=TEXT, fontsize=10)

    def update(frame: int):
        left = max(0, frame - trail_length + 1)
        right = frame + 1

        for pid in pids:
            segment = tracks[pid][left:right]
            current = tracks[pid][frame]
            seen = tracks[pid][:right]

            if dim == 1:
                s = steps[left:right]
                y = [p[0] for p in segment]
                trails[pid].set_data(s, y)
                dots[pid].set_data([steps[frame]], [current[0]])
                seen_s = steps[:right]
                seen_y = [p[0] for p in seen]
                visited[pid].set_offsets(list(zip(seen_s, seen_y)))
            elif dim == 2:
                x = [p[0] for p in segment]
                y = [p[1] for p in segment]
                trails[pid].set_data(x, y)
                dots[pid].set_data([current[0]], [current[1]])
                seen_x = [p[0] for p in seen]
                seen_y = [p[1] for p in seen]
                visited[pid].set_offsets(list(zip(seen_x, seen_y)))
            else:
                x = [p[0] for p in segment]
                y = [p[1] for p in segment]
                z = [p[2] for p in segment]
                trails[pid].set_data(x, y)
                trails[pid].set_3d_properties(z)
                dots[pid].set_data([current[0]], [current[1]])
                dots[pid].set_3d_properties([current[2]])
                seen_x = [p[0] for p in seen]
                seen_y = [p[1] for p in seen]
                seen_z = [p[2] for p in seen]
                visited[pid]._offsets3d = (seen_x, seen_y, seen_z)

        step_text.set_text(f"Step: {steps[frame]}")

        artists = [step_text]
        artists.extend(trails.values())
        artists.extend(dots.values())
        artists.extend(visited.values())
        return artists

    animation = FuncAnimation(
        fig,
        update,
        frames=len(steps),
        interval=max(1, int(1000 / fps)),
        blit=False,
        repeat=True,
    )

    sim_outdir.mkdir(parents=True, exist_ok=True)
    output_path = sim_outdir / f"{csv_path.stem}_simulation.gif"
    animation.save(output_path, writer=PillowWriter(fps=fps))
    plt.close(fig)
    return output_path


def main() -> None:
    args = parse_args()
    setup_matplotlib_theme()

    outdir = Path(args.outdir)
    sim_outdir = Path(args.sim_outdir)

    generated_plots = []
    generated_sims = []

    for input_path in args.input:
        csv_path = Path(input_path)
        if not csv_path.exists():
            raise SystemExit(f"Input file not found: {csv_path}")

        plot_path = plot_file(csv_path, outdir, args.dpi, args.show)
        generated_plots.append(plot_path)

        _, grouped, _, _, _, dim = extract_plot_data(csv_path)
        if dim == 1 and len(grouped) >= 2:
            closer_plot = plot_1d_closer_distance(csv_path, outdir, args.dpi, args.show)
            if closer_plot is not None:
                generated_plots.append(closer_plot)

        if args.simulate:
            sim_path = create_simulation(csv_path, sim_outdir, args.fps, args.trail_length)
            generated_sims.append(sim_path)

    print("Generated clean plot files:")
    for path in generated_plots:
        print(f"- {path}")

    if generated_sims:
        print("Generated simulation GIF files:")
        for path in generated_sims:
            print(f"- {path}")


if __name__ == "__main__":
    main()
