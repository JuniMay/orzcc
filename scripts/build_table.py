import os
import re
import pandas as pd

# Regular expression pattern to match time format
time_pattern = re.compile(r"(\d+)H-(\d+)M-(\d+)S-(\d+)us")

# Function to parse a log file and extract the time data
def parse_log_file(filename):
    data = {}
    with open(f"./tests/testcases/{filename}", 'r') as file:
        for line in file:
            if ':' in line:
                name, time_str = line.split(':')
                name = name.strip()
                time_match = time_pattern.search(time_str)
                if time_match:
                    hours, minutes, seconds, microseconds = map(int, time_match.groups())
                    total_time_in_seconds = hours * 3600 + minutes * 60 + seconds + microseconds / 1e6
                    data[name] = total_time_in_seconds
    return data

# List all log files in the directory
log_files = [f for f in os.listdir("./tests/testcases/") if f.endswith('.txt')]

# Initialize an empty DataFrame
df = pd.DataFrame()

# Process each log file and add the data to the DataFrame
for log_file in log_files:
    log_data = parse_log_file(log_file)
    df[log_file] = pd.Series(log_data)

# Replace missing values with a hyphen
df.fillna("-", inplace=True)
df.index.name = "Log Item"

# Save the DataFrame as a CSV file
df.to_csv("./tests/testcases/table.csv")

print("Log files combined into 'table.csv'")
