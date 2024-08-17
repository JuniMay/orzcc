import os
import hashlib

def hash_file(file_path):
    """Calculate the hash value of a file."""
    hasher = hashlib.sha256()
    if os.path.exists(file_path):
        with open(file_path, 'rb') as f:
            buf = f.read()
            hasher.update(buf)
    else:
        # If file does not exist, return hash of empty string
        hasher.update(b'')
    return hasher.hexdigest()

def remove_duplicate_files(directory):
    # Get a list of all files with the .sy extension in the directory
    files = [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.sy') and os.path.isfile(os.path.join(directory, f))]

    hash_dict = {}
    
    for file_path in files:
        # Hash the .sy file
        sy_hash = hash_file(file_path)
        
        # Hash the corresponding .in and .out files
        file_name = os.path.splitext(file_path)[0]
        in_file = file_name + '.in'
        out_file = file_name + '.out'
        
        in_hash = hash_file(in_file)
        out_hash = hash_file(out_file)
        
        # Combine the three hashes into a tuple
        combined_hash = (sy_hash, in_hash, out_hash)
        
        if combined_hash in hash_dict:
            # If the combined hash value exists, remove the duplicate files
            os.remove(file_path)
            print(f"Removed duplicate file: {file_path}")
            
            if os.path.exists(out_file):
                os.remove(out_file)
                print(f"Removed duplicate file: {out_file}")
            
            if os.path.exists(in_file):
                os.remove(in_file)
                print(f"Removed duplicate file: {in_file}")
        else:
            hash_dict[combined_hash] = file_path

# Example usage:
# directory = '/Users/fengsicheng/Desktop/orzcc/tests/testcases/merged_union/function'
directory = '/Users/fengsicheng/Desktop/orzcc/tests/testcases/merged_union/performance'
remove_duplicate_files(directory)
