# ----------------------------------------------------------------------------
# Client data migration — GDPR‑safe demo (self‑contained)
# ------------------------------------------------------
packages <- c("tidyverse","readxl","writexl","openxlsx","stringr")
lapply(packages, require, character.only = TRUE)

# ----------------------------------------------------------------------------
# Config
# ----------------------------------------------------------------------------
SAVE_OUTPUTS <- FALSE                     # set TRUE to write example outputs
OUTPUT_DIR   <- "outputs"                 # created on demand

if (SAVE_OUTPUTS) dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------
parse_mixed_date <- function(date_vec) {
  # Accepts character like "31/12/2020" or "2020-12-31" or numeric Excel serial
  parsed <- suppressWarnings(as.Date(date_vec, format = "%d/%m/%Y"))
  need   <- is.na(parsed)
  parsed[need] <- suppressWarnings(as.Date(date_vec[need], format = "%Y-%m-%d"))
  need   <- is.na(parsed)
  # Excel origin 1899-12-30 (as used by Windows Excel)
  parsed[need] <- suppressWarnings(as.Date(as.numeric(date_vec[need]), origin = "1899-12-30"))
  parsed
}

clean_paragraph <- function(text) {
  if (is.na(text) || str_trim(text) == "") return(NA_character_)
  sentences <- unlist(str_split(str_trim(text), "(?<=[.!?])\\s+"))
  sentences <- str_to_sentence(sentences)
  paste(sentences, collapse = " ")
}

clean_list_item <- function(text) {
  if (is.na(text) || str_trim(text) == "") return(NA_character_)
  text <- str_remove(text, "\\s*\\[.*$")            # remove trailing bracketed notes
  text <- str_replace_all(text, "\\s*-\\s*", ": ")  # hyphen to colon
  str_to_sentence(str_trim(text))
}

build_contact_description <- function(
    user_name, username_old,
    p1, p2, p3,
    subject, details,
    sa1, sa2, sa3,
    action_details, action_notes, individual_action,
    so1, so2, so3,
    outcome_details, referral_notes
) {
  lines <- c()
  if (!is.na(user_name) && user_name == "Previous Database User" && !is.na(username_old)) {
    lines <- c(lines, paste0("STAFF NAME: ", username_old), "")
  }
  purposes <- list(clean_list_item(p1), clean_list_item(p2), clean_list_item(p3))
  if (any(!sapply(purposes, is.na))) {
    lines <- c(lines, "PURPOSE OF VISIT:")
    for (i in seq_along(purposes)) if (!is.na(purposes[[i]])) lines <- c(lines, paste0(i, "- ", purposes[[i]]))
    lines <- c(lines, "")
  }
  subj <- clean_paragraph(subject); if (!is.na(subj)) lines <- c(lines, "SUBJECT:", subj, "")
  det  <- clean_paragraph(details); if (!is.na(det))  lines <- c(lines, "DETAILS:", det, "")
  actions <- list(clean_list_item(sa1), clean_list_item(sa2), clean_list_item(sa3))
  if (any(!sapply(actions, is.na))) {
    lines <- c(lines, "ACTIONS:")
    for (i in seq_along(actions)) if (!is.na(actions[[i]])) lines <- c(lines, paste0(i, "- ", actions[[i]]))
    lines <- c(lines, "")
  }
  ad <- clean_paragraph(action_details);    if (!is.na(ad))  lines <- c(lines, "ACTION DETAILS:", ad, "")
  an <- clean_paragraph(action_notes);      if (!is.na(an))  lines <- c(lines, "ACTION NOTES:", an, "")
  ia <- clean_paragraph(individual_action); if (!is.na(ia))  lines <- c(lines, "INDIVIDUAL ACTION:", ia, "")
  outcomes <- list(clean_list_item(so1), clean_list_item(so2), clean_list_item(so3))
  if (any(!sapply(outcomes, is.na))) {
    lines <- c(lines, "SERVICE OUTCOMES:")
    for (i in seq_along(outcomes)) if (!is.na(outcomes[[i]])) lines <- c(lines, paste0(i, "- ", outcomes[[i]]))
    lines <- c(lines, "")
  }
  od <- clean_paragraph(outcome_details); if (!is.na(od)) lines <- c(lines, "OUTCOME DETAILS:", od, "")
  rn <- clean_paragraph(referral_notes);  if (!is.na(rn)) lines <- c(lines, "REFERRAL NOTES:", rn)
  paste(lines, collapse = "\n")
}

# ----------------------------------------------------------------------------
# Synthetic demo inputs (replace with readxl::read_excel in real usage)
# ----------------------------------------------------------------------------
set.seed(24)

demo_ind <- tibble(
  id = 1:8,
  updated_at = as.POSIXct(c("2018-12-31 23:59:59","2019-01-01 00:00:01","2020-05-12 12:30:00",
                            "2022-07-23 09:15:00","2018-06-01 10:00:00","2019-03-03 08:00:00",
                            "2021-02-14 14:00:00","2024-11-05 17:45:00")),
  title = c("mr.", NA, "Ms.", "Mx.", NA, "mrs", NA, "Dr."),
  last_name = c("smith","KHAN","williams","O'NEILL","zhang","n'diaye","garcia","MacLeod"),
  first_name = c("jane","Amir","louise","rory","xiao","amina","carlos","fiona"),
  phone_mobile = c("07123 456789"," 07400 111222 ",NA,"07890 000999","+44 7700 900123","","07111111111","(07000)123456"),
  alternative_phone = c(NA, "0161 123 4567", NA, NA, NA, NA, "0161 765 4321", NA),
  alternative_phone_name = c(NA, "Caseworker", NA, NA, NA, NA, "Partner", NA),
  gender = c("male","female","unspecified","other","female","male","female","unspecified"),
  main_language = c("english","ARABIC","Spanish","french",NA,"tigrinya"," english ","Urdu"),
  nationality = c("uk", "Syria", NA, "France", "China", "Senegal", "Spain", "UK"),
  marital_status = c("single","married",NA," civil partnership ","divorced","unknown","married","single"),
  birthday = as.Date(c("1990-01-01","1987-05-05","1995-10-10",NA,"1999-02-02","1980-03-03","1992-04-04","1988-08-08")),
  created_at = as.Date(c("2018-01-01","2019-02-02","2020-01-01","2022-02-02","2017-06-01","2019-04-01","2021-01-01","2024-12-01"))
)

demo_contacts <- tibble(
  id = 1001:1008,
  individual_id = c(1,2,2,3,4,5,6,8),
  contact_date = c("31/12/2018","01/01/2019","2020-06-01","2022-07-24", "2018-06-02", "2019-03-05", "2021-02-15", "2024-12-31"),
  ServiceType = c("Advice","Drop-in","Advice","Outreach","Advice","Drop-in","Advice","Advice"),
  UserName = c("j.smith","a.khan","Previous Database User","r. oneill","systems","amina.n","amina.n","fiona.m"),
  PurposeOfVisit1 = c("benefits - uc", NA, "housing - appointment", NA, NA, NA, "immigration - follow up", NA),
  PurposeOfVisit2 = c(NA, NA, NA, NA, NA, NA, NA, NA),
  PurposeOfVisit3 = c(NA, NA, NA, NA, NA, NA, NA, NA),
  subject = c("Universal credit query.", NA, "Housing update", NA, NA, NA, "Status check", NA),
  details = c("Client asked about uc timeline.", NA, "Discussed banding and application.", NA, NA, NA, "Checked BRP and expiry.", NA),
  ServiceAction1 = c("filled form - uc50", NA, "advised - housing options", NA, NA, NA, "called - home office", NA),
  ServiceAction2 = NA,
  ServiceAction3 = NA,
  action_details = c(NA, NA, "We reviewed documents.", NA, NA, NA, "Chased via helpline.", NA),
  action_notes = NA,
  individual_action = c(NA, NA, "Bring paperwork next visit.", NA, NA, NA, NA, NA),
  ServiceOutcome1 = c("resolved - query", NA, "in progress - application", NA, NA, NA, "in progress - verification", NA),
  ServiceOutcome2 = NA,
  ServiceOutcome3 = NA,
  outcome_details = c(NA, NA, "Awaiting response.", NA, NA, NA, "Pending update.", NA),
  referral_notes = NA
)

# Tiny mapping tables (synthetic)
validation_languages <- tibble(language = c("English","Arabic","Spanish","French","Tigrinya","Urdu"))
validation_projects  <- tibble(old = c("Advice","Drop-in","Outreach"), new = c("Advice","Drop-in","Outreach"))
user_map <- tibble(old = c("j.smith","a.khan","r. oneill","amina.n","fiona.m"),
                   new = c("J Smith","A Khan","R O'Neill","A N","F M"))

date_threshold <- as.POSIXct("2019-01-01 00:00:00")

# ----------------------------------------------------------------------------
# Stage 1 — Select individuals active since threshold (profile updated OR contact)
# ----------------------------------------------------------------------------
recent_ids_from_updates <- demo_ind %>%
  filter(updated_at >= date_threshold) %>% pull(id)

recent_ids_from_contacts <- demo_contacts %>%
  mutate(contact_date = parse_mixed_date(contact_date)) %>%
  filter(contact_date >= as.Date(date_threshold)) %>% pull(individual_id)

all_recent_ids <- union(recent_ids_from_updates, recent_ids_from_contacts)

individuals <- demo_ind %>% filter(id %in% all_recent_ids)

# ----------------------------------------------------------------------------
# Stage 2 — Standardise fields (case, spacing, categories)
# ----------------------------------------------------------------------------
individuals <- individuals %>%
  mutate(
    title = case_when(
      is.na(title) ~ NA_character_,
      TRUE ~ str_remove(str_to_sentence(str_trim(title)), "\\.$")
    ),
    last_name  = str_to_upper(str_trim(last_name)),
    first_name = str_to_title(str_trim(first_name)),
    
    phone_mobile        = str_replace_all(phone_mobile, " ", ""),
    alternative_phone   = str_replace_all(alternative_phone, " ", ""),
    alternative_phone_name = str_trim(alternative_phone_name),
    
    gender = case_when(
      gender == "male" ~ "Male",
      gender == "female" ~ "Female",
      gender %in% c("unspecified","other","unknown") ~ "Unknown",
      TRUE ~ str_to_title(gender)
    ),
    
    main_language = str_to_title(str_trim(main_language)),
    main_language = if_else(main_language %in% validation_languages$language, main_language, "Unknown"),
    
    nationality = str_to_title(str_trim(nationality)),
    nationality = replace_na(nationality, "Unknown"),
    
    marital_status = str_to_sentence(str_trim(marital_status)),
    marital_status = replace_na(marital_status, "Unknown")
  )

# ----------------------------------------------------------------------------
# Stage 3 — Mapping to a target structure (Organisations & People)
# ----------------------------------------------------------------------------
orgs <- tibble(
  `Org/Person ID` = individuals$id,
  Name                  = individuals$last_name,
  Title                 = individuals$title,
  `Other Names`         = individuals$first_name,
  `Marital Status`      = individuals$marital_status,
  `Main Phone Number`   = individuals$phone_mobile,
  `Alternative Phone Number` = individuals$alternative_phone,
  Gender                = individuals$gender,
  `Date of Birth`       = individuals$birthday,
  `National of`         = individuals$nationality,
  `First Referral Date` = individuals$created_at,
  `Also Known As`       = NA_character_,
  `Alternative Number Belongs To` = individuals$alternative_phone_name,
  `Main Language`       = individuals$main_language,
  `Is a Client`         = "Y",
  Active                = "Y"
)

# ----------------------------------------------------------------------------
# Stage 4 — Contacts mapping (recoding project + username, building description)
# ----------------------------------------------------------------------------
contacts <- demo_contacts %>%
  mutate(
    contact_date = parse_mixed_date(contact_date)
  ) %>%
  left_join(validation_projects, by = c("ServiceType" = "old")) %>%
  mutate(ServiceType = coalesce(new, ServiceType)) %>%
  select(-new) %>%
  mutate(UserName = str_trim(str_replace_all(UserName, "  ", " "))) %>%
  left_join(user_map, by = c("UserName" = "old")) %>%
  rename(Username_old = UserName, UserName = new)

contacts_mig <- contacts %>%
  transmute(
    `Org/Person ID` = individual_id,
    `Project Name`  = ServiceType,
    `Contact Date`  = contact_date,
    `User Name`     = UserName,
    `Contact Description` = mapply(
      build_contact_description,
      UserName, Username_old,
      PurposeOfVisit1, PurposeOfVisit2, PurposeOfVisit3,
      subject, details,
      ServiceAction1, ServiceAction2, ServiceAction3,
      action_details, action_notes, individual_action,
      ServiceOutcome1, ServiceOutcome2, ServiceOutcome3,
      outcome_details, referral_notes
    ),
    `Type of Org/Person` = "Client",
    Template = "Interactions",
    Stage    = "Notes From Old Database",
    `Old System ID` = id
  ) %>%
  mutate(`Contact Date` = format(as.Date(`Contact Date`), "%d/%m/%Y"))

# Example integrity check: look for unknown project names
unknown_projects <- contacts_mig %>% filter(!`Project Name` %in% validation_projects$new)

# ----------------------------------------------------------------------------
# Save (off by default)
# ----------------------------------------------------------------------------
if (SAVE_OUTPUTS) {
  write_xlsx(orgs, file.path(OUTPUT_DIR, "organisations_people_demo.xlsx"))
  write_xlsx(contacts_mig, file.path(OUTPUT_DIR, "contacts_demo.xlsx"))
}
