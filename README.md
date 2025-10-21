# Client Data Migration — GDPR‑Safe Demo (R)

This repository contains a **slimmed‑down, fully anonymised example** of a client data cleaning and migration workflow, inspired by a real project with a charity. The purpose is to showcase R skills in **data wrangling, recoding, validation, and migration mapping** without exposing any personal or sensitive information.

---

## Important Notes
- **No real data is included.** All data in this repo is **synthetic dummy data** generated inline within the script.
- This example is a **condensed version** of a larger workflow originally written for a charity data migration project.
- It demonstrates the same types of operations (date parsing, case/whitespace cleaning, mapping codes, building combined descriptions) but only on toy examples.

---

## What the Script Shows
- Generating small demo datasets in‑memory to mimic real inputs.
- Cleaning and standardising fields (dates, names, phones, gender, etc.).
- Applying simple validation/mapping tables.
- Building a **Contact Description** from multiple free‑text fields using helper functions.
- Mapping cleaned data into **target structures** (`Organisations & People`, `Contacts`).

---

## License
This demo is provided for educational and portfolio purposes only.
