import pandas as pd
from ethnicolr import census_ln
from ethnicolr import pred_fl_reg_name, pred_fl_reg_ln, pred_census_ln
from ethnicolr import pred_wiki_name

def predict_race_fl(file):
    file = str(file)
    names = pd.read_csv(file)
    pred_df = pred_fl_reg_name(names, "rest_name", "first_name")
    return pred_df

def predict_race_census(file):
    file = str(file)
    names = pd.read_csv(file)
    pred_df = pred_census_ln(names, "rest_name")
    return pred_df


def print_file(file):
    print(str(file))