#-------------------------------------------------------------------------------
# Add client to BTW polls 1998-2021 from wahlrecht.de and civey 
#
# Source: https://dawum.de/Bundestag/Civey/; https://www.wahlrecht.de
#
# Author: Sina Chen
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(dplyr)

# Data --------------------------------------------------------------------

polls <- readRDS("data/german_bundestag/btw_polls_1998_2021_civey.RDS")


#-------------------------------------------------------------------------------


# Add client --------------------------------------------------------------

polls_client <- polls %>%
  mutate(client = case_when(institute == 'allensbach' ~ 'faz',
                            institute == 'emnid' &
                              date <= as.Date('19.08.2004', '%d.%m.%Y') &
                              !(date %in% c(as.Date('2002-06-25'),
                                            as.Date('2002-07-01'),
                                            as.Date('2002-07-30'),
                                            as.Date('2002-08-06'),
                                            as.Date('2002-08-21'),
                                            as.Date('2002-08-27'),
                                            as.Date('2002-06-10'),
                                            as.Date('2002-09-14'))) ~ 'ntv',
                            institute == 'emnid' &
                              date %in% c(as.Date('2002-06-25'),
                                          as.Date('2002-07-01'),
                                          as.Date('2002-07-30'),
                                          as.Date('2002-08-06'),
                                          as.Date('2002-08-21'),
                                          as.Date('2002-08-27')) ~ 'die_welt',
                            institute == 'emnid' &
                              date %in% c(as.Date('2002-06-10'),
                                          as.Date('2002-09-14'),
                                          as.Date('2005-09-10')) |
                              institute == 'insa' &
                              date %in% c(as.Date('2013-04-21'),
                                          as.Date('2013-05-17'),
                                          as.Date('2013-06-01'),
                                          as.Date('2013-09-15'),
                                          as.Date('2017-11-23'),
                                          as.Date('2018-09-21')) ~ 'focus',
                            institute == 'emnid' &
                              date > as.Date('19.08.2004', '%d.%m.%Y') &
                              date <= as.Date('09.03.2005', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date >= as.Date('27.06.2006', '%d.%m.%Y') &
                              date <= as.Date('20.12.2006', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date >= as.Date('04.07.2007', '%d.%m.%Y') &
                              date <= as.Date('15.08.2007', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date >= as.Date('06.08.2008', '%d.%m.%Y') &
                              date <= as.Date('27.08.2008', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date %in% c(as.Date('2005-05-18'), as.Date('2006-06-14'),
                                          as.Date('2007-04-11'), as.Date('2009-04-08')) ~ 'emnid',
                            institute == 'emnid' &
                              date >= as.Date('16.03.2005', '%d.%m.%Y') &
                              date <= as.Date('31.05.2005', '%d.%m.%Y') &
                              date != as.Date('18.05.2005', '%d.%m.%Y') |
                              date == as.Date('05.07.2005', '%d.%m.%Y') ~ 'ddp',
                            institute == 'emnid' &
                              date %in% c(as.Date('2005-06-04'), as.Date('2005-06-11'),
                                          as.Date('2005-06-18'), as.Date('2005-06-25'),
                                          as.Date('2005-07-02'), as.Date('2005-07-09'),
                                          as.Date('2005-07-16'), as.Date('2005-08-03'),
                                          as.Date('2005-08-10'), as.Date('2005-08-16'),
                                          as.Date('2005-08-23'), as.Date('2005-08-30'),
                                          as.Date('2005-09-07')) ~ 'ber_morgenpost',
                            institute == 'emnid' &
                              date == as.Date('15.07.2005', '%d.%m.%Y') ~ 'leipziger_volkszeitung',
                            institute == 'emnid' &
                              date %in% c(as.Date('2005-07-22'), as.Date('2005-07-29'),
                                          as.Date('2005-08-04'), as.Date('2005-11-08'),
                                          as.Date('2005-08-18'), as.Date('2005-08-25'),
                                          as.Date('2005-09-01'), as.Date('2005-09-08'),
                                          as.Date('2006-06-20'), as.Date('2006-1-28'))  |
                              institute == 'emnid' &
                              date >= as.Date('13.09.2005', '%d.%m.%Y') &
                              date <= as.Date('07.06.2006', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date >= as.Date('30.01.2007', '%d.%m.%Y') &
                              date <= as.Date('26.06.2007', '%d.%m.%Y') &
                              date != as.Date('20.05.2007', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date >= as.Date('21.08.2007', '%d.%m.%Y') &
                              date <= as.Date('18.12.2007', '%d.%m.%Y') &
                              date != as.Date('18.11.2007', '%d.%m.%Y')|
                              institute == 'emnid' &
                              date >= as.Date('08.01.2008', '%d.%m.%Y') &
                              date < as.Date('06.08.2008', '%d.%m.%Y') &
                              date != as.Date('02.03.2008', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date > as.Date('27.08.2008', '%d.%m.%Y') &
                              date <= as.Date('23.12.2008', '%d.%m.%Y') |
                              institute == 'emnid' &
                              date >= as.Date('07.01.2009', '%d.%m.%Y') &
                              date <= as.Date('10.11.2010', '%d.%m.%Y') &
                              date != as.Date('08.04.2009', '%d.%m.%Y')  ~ 'n24',
                            institute == 'emnid' &
                              date %in% c(as.Date('2007-05-20'), as.Date('2007-11-18'),
                                          as.Date('2008-03-02')) |
                              institute == 'emnid' &
                              date >= as.Date('14.11.2010', '%d.%m.%Y') &
                              date <= as.Date('22.09.2013', '%d.%m.%Y') ~ 'bild_am_so',
                            institute == 'emnid' &
                              date > as.Date('22.09.2013', '%d.%m.%Y') ~ 'bild_am_so,emnid',
                            institute == 'forsa' &
                              date <= as.Date('06.03.2002', '%d.%m.%Y') ~ 'die_woche',
                            institute == 'forsa' &
                              date >= as.Date('13.03.2002', '%d.%m.%Y') &
                              date < as.Date('06.11.2017', '%d.%m.%Y') &
                              date != as.Date('29.06.2017', '%d.%m.%Y') ~ 'stern,rtl',
                            institute == 'forsa' &
                              date >= as.Date('06.11.2017', '%d.%m.%Y') ~ 'rtl,ntv',
                            institute == 'forsa' &
                              date == as.Date('29.06.2017', '%d.%m.%Y') ~ 'rtl',
                            institute == 'politbarometer' ~ 'zdf',
                            institute == 'gms' ~ 'gms',
                            institute == 'dimap' &
                              date <= as.Date('19.12.1998', '%d.%m.%Y') ~ 'bild',
                            institute == 'dimap' &
                              date > as.Date('19.12.1998', '%d.%m.%Y') &
                              date <= as.Date('24.12.1999', '%d.%m.%Y')~ 'bild, mdr',
                            institute == 'dimap' &
                              date > as.Date('24.12.1999', '%d.%m.%Y') &
                              date <= as.Date('23.12.2004', '%d.%m.%Y') &
                              !(date  %in% c(as.Date('2002-04-10'),
                                             as.Date('2002-05-15'),
                                             as.Date('2002-09-04')))~ 'mdr,dimap',
                            institute == 'dimap' &
                              date  %in% c(as.Date('2002-04-10'),
                                           as.Date('2002-05-15'),
                                           as.Date('2002-09-04')) ~ 'br',
                            institute == 'dimap' &
                              date > as.Date('23.12.2004', '%d.%m.%Y') ~ 'ard,dimap',
                            institute == 'insa' &
                              !(date %in% c(as.Date('2013-04-21'),
                                            as.Date('2013-05-17'),
                                            as.Date('2013-06-01'),
                                            as.Date('2013-09-15'),
                                            as.Date('2017-11-23'),
                                            as.Date('2018-09-21'))) ~ 'bild,insa',
                            institute == 'yougov' &
                              date >= as.Date('29.06.2017', '%d.%m.%Y') &
                              date <= as.Date('18.12.2019', '%d.%m.%Y') ~ 'rnd',
                            institute == 'yougov' &
                              date <= as.Date('23.06.2017', '%d.%m.%Y') |
                              institute == 'yougov' &
                              date >= as.Date('06.03.2020', '%d.%m.%Y') ~ 'yougov',
                            institute == 'civey' ~ 'spiegel'
                            ))

# save polls
saveRDS("polls_client", "data/german_bundestag/btw_polls_1998_2021_client.RDS")

