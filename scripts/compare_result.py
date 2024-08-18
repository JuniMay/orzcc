import csv

def compare_rows(csv_file, row1, row2):
    with open(csv_file, 'r') as file:
        reader = csv.reader(file)
        rows = list(reader)
        if row1 < 0 or row1 >= len(rows) or row2 < 0 or row2 >= len(rows):
            print("Invalid row numbers")
            return
        headers = rows[0]
        data1 = rows[row1]
        data2 = rows[row2]
        print(f"Row1 commit message: {data1[1]}Row2 commit message: {data2[1]}")

        for i in range(2, len(headers)):  # skip first 2 
            header = headers[i]
            value1_str = data1[i].replace('(AC)', '').strip()
            value2_str = data2[i].replace('(AC)', '').strip()
            try:
                value1 = float(value1_str)
                value2 = float(value2_str)
                diff = value1 - value2
                print(f"Difference in {header} (row1 - row2): {diff}")
            except ValueError:
                print(f"Error converting values in {header}: '{data1[i]}' and '{data2[i]}'")

csv_file = './tests/testcases/result.csv'

row1 = 45
row2 = 58

compare_rows(csv_file, row1, row2)
