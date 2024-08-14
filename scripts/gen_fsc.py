import os
import shutil

def copy_sy_files(source_dir, destination_dir):
    # Get a list of all files in the source directory
    file_list = os.listdir(source_dir)

    # Iterate over the files
    for file_name in file_list:
        # Check if the file has a .sy extension
        if file_name.endswith('.sy'):
            # Construct the source and destination paths
            source_path = os.path.join(source_dir, file_name)
            destination_path = os.path.join(destination_dir, file_name[:-3] + '.fsc')

            # Copy the file to the destination directory
            shutil.copy2(source_path, destination_path)

# Example usage
source_directory = '/Users/fengsicheng/Desktop/orzcc/tests/sysy/external_test'
destination_directory = '/Users/fengsicheng/Desktop/orzcc/tests/sysy/external_test'

copy_sy_files(source_directory, destination_directory)