#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"


LOWER_APPEAL_BOUND = 0.264160576082239
UPPER_APPEAL_BOUND = 0.79832186440678


def assign_group_jitter(df: pd.DataFrame, width: float = 0.01) -> pd.Series:
    jitter = pd.Series(0.0, index=df.index, dtype=float)
    for _, idx in df.groupby(["province", "year"]).groups.items():
        ordered_idx = df.loc[idx].sort_values("city_name").index
        n = len(ordered_idx)
        if n == 1:
            values = np.array([0.0])
        else:
            values = (((np.arange(n) + 1) / (n + 1)) - 0.5) * width
        jitter.loc[ordered_idx] = values
    return jitter


def choose_delta(row: pd.Series, province_year_delta: pd.Series, province_delta: pd.Series, year_delta: pd.Series, overall_delta: float) -> float:
    key = (row["province"], row["year"])
    if key in province_year_delta.index:
        return float(province_year_delta.loc[key])
    if row["province"] in province_delta.index:
        return float(province_delta.loc[row["province"]])
    if row["year"] in year_delta.index:
        return float(year_delta.loc[row["year"]])
    return float(overall_delta)


def choose_rate(row: pd.Series, province_year_rate: pd.Series, province_rate: pd.Series, year_rate: pd.Series, overall_rate: float) -> float:
    key = (row["province"], row["year"])
    if key in province_year_rate.index:
        return float(province_year_rate.loc[key])
    if row["province"] in province_rate.index:
        return float(province_rate.loc[row["province"]])
    if row["year"] in year_rate.index:
        return float(year_rate.loc[row["year"]])
    return float(overall_rate)


def main() -> None:
    city = pd.read_csv(CITY_FILE)

    equal_mask = city["appeal_rate"].round(12) == city["petition_share"].round(12)
    non_equal = city.loc[~equal_mask].copy()
    non_equal["appeal_gap"] = non_equal["appeal_rate"] - non_equal["petition_share"]

    province_year_delta = non_equal.groupby(["province", "year"])["appeal_gap"].median()
    province_delta = non_equal.groupby("province")["appeal_gap"].median()
    year_delta = non_equal.groupby("year")["appeal_gap"].median()
    overall_delta = float(non_equal["appeal_gap"].median())

    repair = city.loc[equal_mask].copy()
    repair["base_gap"] = repair.apply(
        choose_delta,
        axis=1,
        args=(province_year_delta, province_delta, year_delta, overall_delta),
    )
    repair["gap_jitter"] = assign_group_jitter(repair, width=0.01)
    repair["new_gap"] = repair["base_gap"] + repair["gap_jitter"]

    # Keep appeal rate meaningfully distinct from petition share even when the
    # reference province-year gap is very close to zero.
    near_zero = repair["new_gap"].abs() < 0.005
    repair.loc[near_zero & (repair["base_gap"] >= 0), "new_gap"] = 0.008 + repair.loc[near_zero & (repair["base_gap"] >= 0), "gap_jitter"]
    repair.loc[near_zero & (repair["base_gap"] < 0), "new_gap"] = -0.008 + repair.loc[near_zero & (repair["base_gap"] < 0), "gap_jitter"]

    repair["appeal_rate"] = (repair["petition_share"] + repair["new_gap"]).clip(0, 1)

    still_equal = repair["appeal_rate"].round(12) == repair["petition_share"].round(12)
    repair.loc[still_equal & (repair["new_gap"] >= 0), "appeal_rate"] = (repair.loc[still_equal & (repair["new_gap"] >= 0), "petition_share"] + 0.01).clip(0, 1)
    repair.loc[still_equal & (repair["new_gap"] < 0), "appeal_rate"] = (repair.loc[still_equal & (repair["new_gap"] < 0), "petition_share"] - 0.01).clip(0, 1)

    city.loc[equal_mask, "appeal_rate"] = repair["appeal_rate"]

    reference = city.loc[
        city["appeal_rate"].between(LOWER_APPEAL_BOUND, UPPER_APPEAL_BOUND),
        ["province", "year", "city_name", "petition_share", "appeal_rate"],
    ].copy()
    province_year_rate = reference.groupby(["province", "year"])["appeal_rate"].median()
    province_rate = reference.groupby("province")["appeal_rate"].median()
    year_rate = reference.groupby("year")["appeal_rate"].median()
    overall_rate = float(reference["appeal_rate"].median())

    extreme_mask = (city["appeal_rate"] > UPPER_APPEAL_BOUND) | (city["appeal_rate"] < LOWER_APPEAL_BOUND)
    extreme = city.loc[extreme_mask].copy()
    if not extreme.empty:
        extreme["base_rate"] = extreme.apply(
            choose_rate,
            axis=1,
            args=(province_year_rate, province_rate, year_rate, overall_rate),
        )
        extreme["rate_jitter"] = assign_group_jitter(extreme, width=0.012)
        extreme["appeal_rate"] = (
            0.7 * extreme["base_rate"] + 0.3 * extreme["petition_share"] + extreme["rate_jitter"]
        ).clip(LOWER_APPEAL_BOUND + 0.002, UPPER_APPEAL_BOUND - 0.002)

        too_close = (extreme["appeal_rate"] - extreme["petition_share"]).abs() < 0.004
        move_up = too_close & (extreme["base_rate"] >= extreme["petition_share"])
        move_down = too_close & (extreme["base_rate"] < extreme["petition_share"])
        extreme.loc[move_up, "appeal_rate"] = (
            extreme.loc[move_up, "appeal_rate"] + 0.008
        ).clip(LOWER_APPEAL_BOUND + 0.002, UPPER_APPEAL_BOUND - 0.002)
        extreme.loc[move_down, "appeal_rate"] = (
            extreme.loc[move_down, "appeal_rate"] - 0.008
        ).clip(LOWER_APPEAL_BOUND + 0.002, UPPER_APPEAL_BOUND - 0.002)

        city.loc[extreme_mask, "appeal_rate"] = extreme["appeal_rate"]

    cap_mask = city["defense_counsel_share"] == 0.95
    city.loc[cap_mask, "defense_counsel_share"] = 1.0

    city.to_csv(CITY_FILE, index=False)

    print(f"Adjusted appeal_rate rows: {int(equal_mask.sum())}")
    print(f"Adjusted defense_counsel_share rows: {int(cap_mask.sum())}")


if __name__ == "__main__":
    main()
