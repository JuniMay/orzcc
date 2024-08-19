import os
import re

def parse_time(time_str):
    pattern = r" (\d+)H-(\d+)M-(\d+)S-(\d+)us"
    match = re.match(pattern, time_str)
    if match:
        hours = int(match.group(1))
        minutes = int(match.group(2))
        seconds = int(match.group(3))
        microseconds = int(match.group(4))
        total_seconds = hours * 3600 + minutes * 60 + seconds + microseconds / 1_000_000
        return total_seconds
    else:
        print(f"Error parsing time: {time_str}")  # 添加调试信息
        raise ValueError("Invalid time format")

def extract_total_time(log_directory, output_file, custom_message):
    log_files = sorted([f for f in os.listdir(log_directory) if f.endswith('.log')])
    overall_total_time = 0
    
    with open(output_file, 'w') as output:
        output.write(custom_message + '\n\n')
        
        for log_file in log_files:
            log_path = os.path.join(log_directory, log_file)
            log_file = log_file.replace('.log', '')
            last_total_line = None
            
            with open(log_path, 'r') as file:
                for line in file:
                    if line.startswith('TOTAL'):
                        last_total_line = line.strip()
            
            last_total_line = last_total_line.replace('TOTAL:', '') if last_total_line else None
            if last_total_line:
                print(f"{log_file}: {last_total_line}")
                output.write(f"{log_file}: {last_total_line}\n")
                try:
                    overall_total_time += parse_time(last_total_line)
                except ValueError as e:
                    print(f"Skipping {log_file} due to parsing error: {e}")
        
        output.write(f"\nOverall Total Time: {overall_total_time:.6f} seconds\n")

log_directory = '/Users/fengsicheng/Desktop/orzcc/output' 
output_file = './tests/testcases/time_record_newest.txt'  
custom_message = "Total time for each log file:"

extract_total_time(log_directory, output_file, custom_message)
