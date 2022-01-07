"""
Posprocessing for google open dataset
https://drive.google.com/file/d/10r6cnJ5cJ89fPWCgj7j4LtLBqYN9RiI9/view.
"""

import gzip
import json
import multiprocessing as mp
import os
import pickle
import time
import functools
from os import linesep, path, write

import pandas as pd
from tqdm import tqdm

def apply_filter_production(df):
    """
    Apply filter for Production jobs.
    """
    return df.groupby("collection_id").filter(lambda x: all(x["priority"] >= 120) and all(x["type"] not in [4, 5, 6, 7, 8]))

def apply_filter_batch(df):
    """
    Apply filter for Batch jobs.
    """
    return df.groupby("collection_id").filter(lambda x: all(x["priority"] <= 119) and all(x["type"] not in [4, 5, 7, 8]))

def normalize(line):
    """
    Convert json to pd.DataFrame
    """
    return pd.json_normalize(json.loads(line), max_level=2)

def convert_to_df(lines):
    """
    Convert lines to DataFrame.
    """
    temp_df = []
    for line in tqdm(lines):
        temp_df.extend(list(normalize(line)))
    temp_df = pd.concat(temp_df, sort = False)
    temp_df["collection_id"] = temp_df["collection_id"].astype(int)
    temp_df["priority"] = temp_df["priority"].astype(int)
    temp_df["type"] = temp_df["type"].astype(int)
    return temp_df

def write_df(df, target_fp):
    """
    Write parsed Dataframe.
    """
    if os.path.exists(target_fp):
        df.to_csv(target_fp, mode = 'a', header = False)
    else:
        df.to_csv(target_fp, header = True)

if __name__ == "__main__":
    HEAD_PATH = "/mnt/scratch/"
    PATH = HEAD_PATH + "google_2019_data/"

    st = time.time()
    job_events = sorted(os.listdir(path + "job_events"))

    TARGET_BATCH_FILE_NAME = "job_events_batch_df.csv"
    TARGET_PRODUCTION_FILE_NAME = "job_events_production_df.csv"

    TARGET_BATCH_FILE_PATH = PATH + "parsed_job_events/" + TARGET_BATCH_FILE_NAME
    TARGET_PRODUCTION_FILE_PATH = PATH + "parsed_job_events/" + TARGET_PRODUCTION_FILE_NAME

    MAX_READ_SIZE = 1024

    selected_batch_job_ids = pd.Series()
    selected_production_job_ids = pd.Series()
    for f in tqdm(job_events):
        r = gzip.open(path + 'job_events' + '/' + f, 'rt')
        r.seek(0, 0)
        while True:
            l = r.readlines(MAX_READ_SIZE)
            if not l:
                break
            df = convert_to_df(l)
            batch_df = apply_filter_batch(df)
            production_df = apply_filter_production(df)

            write_df(batch_df, TARGET_BATCH_FILE_NAME)
            write_df(production_df, TARGET_PRODUCTION_FILE_NAME)

            selected_batch_job_ids.append(batch_df.loc[:,"collection_id"], ignore_index=True)
            selected_production_job_ids.append(production_df.loc[:,"collection_id"], ignore_index=True)
    
    selected_batch_job_ids = selected_batch_job_ids.unique()
    selected_production_job_ids = selected_production_job_ids.unique()
    
    print("Total Number of Batch Jobs is " + selected_batch_job_ids.unique().size())
    print("Total Number of Production Jobs is " + selected_production_job_ids.unique().size())
    et = time.time()

    print("Filtering Job Ids took" , (et - st) / 60," minutes.")
    