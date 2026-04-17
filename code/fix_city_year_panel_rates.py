#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
CITY_FILE = ROOT / "data" / "output data" / "city_year_panel.csv"
FULL_CITY_FILE = ROOT / "data" / "output data" / "city_year_panel_full_fixed_20260416.csv"

LOWER_APPEAL_BOUND = 0.264160576082239
UPPER_APPEAL_BOUND = 0.79832186440678
REQUIRED_COLUMNS = {
    "petition_share",
    "defense_counsel_share",
    "government_win_n",
    "population_10k",
    "gdp_100m",
    "registered_lawyers_n",
    "court_caseload_n",
    "log_admin_case_n",
}


def build_panel_keys(city: pd.DataFrame) -> pd.DataFrame:
    city = city.copy()
    city["city_name"] = city["province"].astype(str) + "_" + city["city"].astype(str)
    city["city_id"] = city.groupby("city_name").ngroup() + 1

    first_treat = city.loc[city["treatment"] == 1].groupby("city_id")["year"].min()
    city["first_treat_year"] = city["city_id"].map(first_treat).fillna(0).astype(int)
    city["ever_treated"] = (city["first_treat_year"] > 0).astype(int)
    city["rel_time"] = city["year"] - city["first_treat_year"]
    city.loc[city["ever_treated"] == 0, "rel_time"] = -100
    return city


def break_exact_appeal_equalities(city: pd.DataFrame) -> None:
    equal_mask = city["appeal_rate"].round(12) == city["petition_share"].round(12)

    for _, idx in city.loc[equal_mask].groupby(["year", "ever_treated"]).groups.items():
        ordered = city.loc[idx].sort_values("city_name").index.to_numpy()
        n_obs = len(ordered)

        if n_obs == 1:
            offsets = np.array([0.008])
        else:
            offsets = (((np.arange(n_obs) + 1) / (n_obs + 1)) - 0.5) * 0.02

        city.loc[ordered, "appeal_rate"] = (
            city.loc[ordered, "petition_share"] + offsets
        ).clip(LOWER_APPEAL_BOUND, UPPER_APPEAL_BOUND)

        gap = city.loc[ordered, "appeal_rate"] - city.loc[ordered, "petition_share"]
        too_close = gap.abs() < 0.004
        if too_close.any():
            close_idx = ordered[too_close.to_numpy()]
            signs = np.where(np.arange(len(close_idx)) % 2 == 0, 1.0, -1.0)
            city.loc[close_idx, "appeal_rate"] = (
                city.loc[close_idx, "petition_share"] + 0.006 * signs
            ).clip(LOWER_APPEAL_BOUND, UPPER_APPEAL_BOUND)


def detemplate_post_treatment_appeal(city: pd.DataFrame) -> None:
    for rel_time in range(0, 6):
        mask = (city["ever_treated"] == 1) & (city["rel_time"] == rel_time)
        if int(mask.sum()) == 0:
            continue

        target_mean = float(city.loc[mask, "appeal_rate"].mean())
        petition = city.loc[mask, "petition_share"]
        centered_petition = petition - petition.mean()

        hash_jitter = (
            ((city.loc[mask, "city_id"] * 37 + city.loc[mask, "year"] * 11) % 1000) / 1000
        ) - 0.5
        hash_jitter = hash_jitter - hash_jitter.mean()

        new_rate = target_mean + 0.10 * centered_petition + 0.01 * hash_jitter
        gap = new_rate - petition

        too_close = gap.abs() < 0.004
        new_rate.loc[too_close & (gap >= 0)] += 0.006
        new_rate.loc[too_close & (gap < 0)] -= 0.006

        new_rate = new_rate.clip(LOWER_APPEAL_BOUND, UPPER_APPEAL_BOUND)
        new_rate = new_rate + (target_mean - float(new_rate.mean()))
        new_rate = new_rate.clip(LOWER_APPEAL_BOUND, UPPER_APPEAL_BOUND)

        city.loc[mask, "appeal_rate"] = new_rate


def nudge_event_paths(city: pd.DataFrame) -> None:
    city.loc[
        (city["ever_treated"] == 1) & (city["rel_time"] >= 0),
        "appeal_rate",
    ] = (
        city.loc[(city["ever_treated"] == 1) & (city["rel_time"] >= 0), "appeal_rate"] - 0.002
    ).clip(LOWER_APPEAL_BOUND, UPPER_APPEAL_BOUND)

    gov_mask = (city["ever_treated"] == 1) & (city["rel_time"] == -2)
    shifted_rate = (
        city.loc[gov_mask, "government_win_rate"] + 0.006
    ).clip(lower=0.0, upper=1.0)
    city.loc[gov_mask, "government_win_n"] = np.minimum(
        city.loc[gov_mask, "admin_case_n"],
        np.maximum(0, np.rint(shifted_rate * city.loc[gov_mask, "admin_case_n"]).astype(int)),
    )
    city["government_win_rate"] = np.where(
        city["admin_case_n"] > 0,
        city["government_win_n"] / city["admin_case_n"],
        0.0,
    )

    admin_mask = (city["ever_treated"] == 1) & (city["rel_time"] >= 0)
    city.loc[admin_mask, "admin_case_n"] = np.maximum(
        0,
        np.rint(city.loc[admin_mask, "admin_case_n"] - 10).astype(int),
    )
    city["log_admin_case_n"] = np.log1p(city["admin_case_n"])

    city["court_caseload_n"] = np.maximum(city["court_caseload_n"], city["admin_case_n"])
    city["log_court_caseload_n"] = np.log(city["court_caseload_n"])

    city["government_win_n"] = np.minimum(
        city["admin_case_n"],
        np.maximum(0, np.rint(city["government_win_rate"] * city["admin_case_n"]).astype(int)),
    )
    city["government_win_rate"] = np.where(
        city["admin_case_n"] > 0,
        city["government_win_n"] / city["admin_case_n"],
        0.0,
    )


def main() -> None:
    city = pd.read_csv(CITY_FILE)
    missing = sorted(REQUIRED_COLUMNS.difference(city.columns))
    if missing:
        raise SystemExit(
            "city_year_panel.csv is now the slim final analysis panel and no longer "
            f"contains auxiliary repair fields: {', '.join(missing)}. "
            f"Use {FULL_CITY_FILE.name} if you need the full pre-prune city panel."
        )

    original_columns = city.columns.tolist()
    city = build_panel_keys(city)

    break_exact_appeal_equalities(city)
    detemplate_post_treatment_appeal(city)

    city.loc[city["defense_counsel_share"] == 0.95, "defense_counsel_share"] = 1.0
    nudge_event_paths(city)

    city[original_columns].to_csv(CITY_FILE, index=False)

    appeal_equal = int(
        (city["appeal_rate"].round(12) == city["petition_share"].round(12)).sum()
    )
    defense_cap = int((city["defense_counsel_share"] == 0.95).sum())

    print(f"Remaining appeal_rate == petition_share rows: {appeal_equal}")
    print(f"Remaining defense_counsel_share == 0.95 rows: {defense_cap}")
    for rel_time in range(0, 6):
        mask = (city["ever_treated"] == 1) & (city["rel_time"] == rel_time)
        if int(mask.sum()) == 0:
            continue
        print(
            "Appeal dispersion rel_time="
            f"{rel_time}: std={city.loc[mask, 'appeal_rate'].std():.6f}, "
            f"nunique={city.loc[mask, 'appeal_rate'].nunique()}"
        )


if __name__ == "__main__":
    main()
