#!/usr/bin/env python3

from __future__ import annotations

from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "data" / "output data"

CITY_FILE = OUT_DIR / "city_year_panel.csv"
ADMIN_FILE = OUT_DIR / "admin_case_level.csv"
NOTE_FILE = OUT_DIR / "city_only_2013_extension_note.md"


def main() -> None:
    city = pd.read_csv(CITY_FILE)
    admin = pd.read_csv(ADMIN_FILE)

    admin_trimmed = admin.loc[admin["year"] >= 2014].copy()
    admin_trimmed = admin_trimmed.sort_values(["province", "city", "year", "case_no"]).reset_index(drop=True)
    admin_trimmed.to_csv(ADMIN_FILE, index=False)

    lines = [
        "# City-Only 2013 Extension",
        "",
        "- `city_year_panel.csv` keeps the added `2013` city-year pre-period so the CS estimator no longer drops cities first treated in `2014`.",
        "- `admin_case_level.csv` is intentionally trimmed back to `2014--2020` because the extra pre-period is not needed for the administrative case-level analysis chain.",
        "",
        "## Current coverage",
        f"- `city_year_panel` years: `{int(city['year'].min())}` to `{int(city['year'].max())}`",
        f"- `city_year_panel` rows: `{len(city):,}`",
        f"- `admin_case_level` years: `{int(admin_trimmed['year'].min())}` to `{int(admin_trimmed['year'].max())}`",
        f"- `admin_case_level` rows: `{len(admin_trimmed):,}`",
        f"- `admin_case_level.case_no` duplicates after trim: `{int(admin_trimmed['case_no'].duplicated().sum())}`",
        "",
    ]
    NOTE_FILE.write_text("\n".join(lines), encoding="utf-8")

    print(f"city_rows={len(city):,}")
    print(f"admin_rows_after_trim={len(admin_trimmed):,}")
    print(f"note_file={NOTE_FILE}")


if __name__ == "__main__":
    main()
