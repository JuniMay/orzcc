import os

def extract_total_time(log_directory):
    # 获取指定目录下的所有.log文件，并按文件名排序
    log_files = sorted([f for f in os.listdir(log_directory) if f.endswith('.log')])
    
    for log_file in log_files:
        log_path = os.path.join(log_directory, log_file)
        log_file = log_file.replace('.log', '')
        last_total_line = None
        
        # 读取文件的所有行
        with open(log_path, 'r') as file:
            for line in file:
                if line.startswith('TOTAL'):
                    last_total_line = line.strip()
        
        # 打印最后一个以'TOTAL:'开头的行
        last_total_line = last_total_line.replace('TOTAL:', '') if last_total_line else None
        if last_total_line:
            print(f"{log_file}: {last_total_line}")

# 示例调用
log_directory = '/Users/fengsicheng/Desktop/orzcc/output'  # 替换为你的日志文件目录
extract_total_time(log_directory)