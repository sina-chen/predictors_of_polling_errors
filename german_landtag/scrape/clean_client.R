### clean clients ###

landtag_polls_wide %>% 
  separate(col = client, into = c("first", "second", "third", "fourth"), sep = ",") %>% 
  mutate(across(c(first, second, third, fourth), ~ str_squish(.x))) %>% 
  mutate(across(c(first, second, third, fourth), ~ tolower(.x))) %>%
  mutate(across(c(first, second, third, fourth), ~ str_replace_all(.x, '\\s', '_'))) %>% 
  mutate(first = clean_client(first)) -> landtag_polls_wide


clean_client <- function(variable){
  case_when(
  variable == "stuttgarterzeitung" ~ "stuttgarter_zeitung",
  variable == "badischezeitung" ~ "badische_zeitung",
  variable == "staats-ministerium" ~ "staatsministerium",
  variable == "stuttgarternachrichten" ~ "stuttgarter_nachrichten",
  variable == "spd-lt-fraktion" ~ "spd_lt_fraktion",
  variable == "schwã¤bischezeitung" ~ "schwäbische_zeitung",
  variable == "spd-ltfraktion" ~ "spd_lt_fraktion",
  variable == "cdu-lv" ~ "cdu_landesverband",
  variable == "sat.1bayern" ~ "sat1_bayern",
  variable == "bayerischerrundfunk" ~ "bayerischer_rundfunk",
  variable == "sã¼ddeutschezeitung" ~ "süddeutsche_zeitung",
  variable == "spd-landesverband" ~ "spd_landesverband",
  variable == "antennebayern" ~ "antenne_bayern",
  variable == "bild_amsonntag" ~ "bild_am_sonntag",
  variable == "agenturbutter" ~ "agentur_butter",
  variable == "quelle:_mã¼nch-ner_merkur" ~ "münchner_merkur",
  variable == "bayerischestaatskanzlei" ~ "bayerische_staatskanzlei",
  variable == "grãœne_lv" ~ "grüne_landesverband",
  variable == "?(quelle:_pnp)" ~ "pnp",
  variable == "fdp_lv" ~ "fdp_landesverband",
  variable == "berlinermorgenpost" ~ "berliner_morgenpost",
  variable == "berlinerzeitung" ~ "berliner_zeitung",
  variable == "berlinerkurier" ~ "berliner_kurier",
  variable == "b.z._(quelle)" ~ "berliner_zeitung",
  variable == "infratestdimap" ~ "infratest_dimap",
  variable == "mã¤rkischeallgemeine" ~ "märkische_allgemeine",
  variable == "spd-lv" ~ "spd_landesverband",
  variable == "die_linke-landesverband" ~ "die_linke_landesverband",
  variable == "spd-lv_(?)" ~ "spd_landesverband",
  variable == "centrum_fã¼rpolit._studien" ~ "centrum_für_politische_studien",
  variable == "cdu-landesverband" ~ "cdu_landesverband",
  variable == "märkischeallgemeine" ~ "märkische_allgemeine",
  variable == "spd-lv" ~ "spd_landesverband",
  variable == "focusquelle" ~ "focus",
  variable == "grãœne-bã¼rg.-fraktion" ~ "grüne_bürg_fraktion",
  variable == "grãœne" ~ "grüne",
  variable == "spd-landes-organisation" ~ "spd_landesverband",
  variable == "hamburgerabendblatt" ~ "hamburger_abendblatt",
  variable == "hamburgermorgenpost" ~ "hamburger_morgenpost",
  variable == "quelle:focus" ~ "focus",
  variable == "universitã¤thamburg" ~ "universität_hamburg",
  variable == "hamburgzwei" ~ "hamburg_zwei",
  variable == "kã¶rber-stiftung" ~ "körber_stiftung",
  variable == "hessischerrundfunk" ~ "hessischer_rundfunk",
  variable == "cdu_lvhessen" ~ "cdu_landesverband",
  variable == "quelle:_cdu-lv_hessen" ~ "cdu_landesverband",
  variable == "frankfurterrundschau" ~ "frankfurter_rundschau",
  variable == "quelle:_dpa" ~ "dpa",
  variable == "frankfurterneue_presse" ~ "frankfurter_neue_presse",
  variable == "fdp-lv" ~ "fdp_landesverband",
  variable == "ostsee-zeitung" ~ "ostsee_zeitung",
  variable == "schwerinervolkszeitung" ~ "schweriner_volkszeitung",
  variable == "antennemeckl.-vorp." ~ "antenne_mv",
  variable == "cdu_mv" ~ "cdu_landesverband",
  variable == "fdp_mv" ~ "fdp_landesverband",
  variable == "spd-lv_mv" ~ "spd_landesverband",
  variable == "staatskanzleimv" ~ "staatskanzlei_mv",
  variable == "hit-radioantenne" ~ "hit-radio_antenne",
  variable == "neuepresse" ~ "neue_presse",
  variable == "cdu_inniedersachsen" ~ "cdu_landesverband",
  variable == "sat.1norddeutschland" ~ "sat.1_norddeutschland",
  variable == "rheinischepost" ~ "rheinische_post",
  variable == "handels-blatt" ~ "handelsblatt",
  variable == "wirtschafts-woche" ~ "wirtschaftswoche",
  variable == "cdu-lvnrw" ~ "cdu_landesverband",
  variable == "fdp-lvnrw" ~ "fdp_landesverband",
  variable == "lreg_nrw" ~ "landesregierung_nrw",
  variable == "westfã¤lischerundschau" ~ "westfälische_rundschau",
  variable == "grãœne_nrw" ~ "grüne_landesverband",
  variable == "kã¶lner_stadt-anzeiger" ~ "kölner_stadt-anzeiger",
  variable == "kã¶lnischerundschau" ~ "kölnische_rundschau",
  variable == "cdu-lvsaar" ~ "cdu_landesverband",
  variable == "saarbrã¼ckerzeitung" ~ "saarbrücker_zeitung",
  variable == "sã¤chsischestaatsregierung" ~ "sächsische_staatsregierung",
  variable == "spd_sachsen" ~ "spd_landesverband",
  variable == "leipzigervolkszeitung" ~ "leipziger_volkszeitung",
  variable == "sã¤chsischezeitung_(quelle)" ~ "sächsische_zeitung",
  variable == "spd-lvsachsen" ~ "spd_landesverband",
  variable == "mitteldeutschezeitung" ~ "mitteldeutsche_zeitung",
  variable == "magdeburgervolksstimme" ~ "magdeburger_volksstimme",
  variable == "die_linkelv_st" ~ "die_linke_landesverband",
  variable == "lã¼beckernachrichten" ~ "lübecker_nachrichten",
  variable == "flensburgertageblatt" ~ "flensburger_tageblatt",
  variable == "lã¼becker_undkieler_nachrichten" ~ "lübecker_und_kieler_nachrichten",
  variable == "thã¼ringerallgemeine" ~ "thüringer_allgemeine",
  variable == "bild*" ~ "bild",
  variable == "bâ€™90/grãœnethã¼ringen" ~ "grüne_landesverband",
  variable == "sã¼dthã¼ringerzeitung" ~ " südthüringe_rzeitung",
  variable == "thã¼ringerstaatskanzlei" ~ "thüringer_staatskanzlei",
  variable == "tollesthã¼ringen" ~ "tolles_thüringen",
  variable == "osterlã¤ndervolkszeitung" ~ "osterländer_volkszeitung",
  variable == "thã¼ringischelandeszeitung" ~ "thüringische_landeszeitung",
  TRUE ~ variable
  )
  
}


landtag_polls_wide %>% 
  separate(col = client, into = c("first", "second", "third", "fourth"), sep = ",") %>% 
  mutate(across(c(first, second, third, fourth), ~ str_squish(.x))) %>% 
  mutate(across(c(first, second, third, fourth), ~ tolower(.x))) %>%
  mutate(across(c(first, second, third, fourth), ~ str_replace_all(.x, '\\s', '_'))) %>% 
  mutate(first = clean_client(first)) -> landtag_polls_wide


landtag_polls_wide %>% 
  filter(state == "baden-wuerttemberg" | state == "bayern" | state == "berlin"
         | state == "brandenburg") %>% 
  ggplot(aes(x = fct_infreq(first))) + 
  geom_bar() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ state, scales = "free") + xlab("client") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.2, size = 3)



landtag_polls_wide %>% 
  filter(state == "bremen" | state == "hamburg" | state == "hessen"
         | state == "mecklenburg-vorpommern") %>% 
  ggplot(aes(x = fct_infreq(first))) + 
  geom_bar() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ state, scales = "free") + xlab("client") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.2, size = 3)

landtag_polls_wide %>% 
  filter(state == "niedersachsen" | state == "nrw" | state == "rheinland-pfalz"
         | state == "saarland") %>% 
  ggplot(aes(x = fct_infreq(first))) + 
  geom_bar() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ state, scales = "free") + xlab("client") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.2, size = 3)

landtag_polls_wide %>% 
  filter(state == "sachsen" | state == "sachsen-anhalt" | state == "schleswig-holstein"
         | state == "thueringen") %>% 
  ggplot(aes(x = fct_infreq(first))) + 
  geom_bar() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ state, scales = "free") + xlab("client") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.2, size = 3)


clean_institute(landtag_polls_wide) -> landtag_polls_wide
