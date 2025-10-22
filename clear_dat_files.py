# clears the content of all .dat files in the directory

import os

def clear_dat_files():
    """
    Clears the contents of all .dat files in the current working directory
    without deleting the files themselves.
    """
    current_dir = os.getcwd()
    cleared = []

    for filename in os.listdir(current_dir):
        if filename.endswith(".dat"):
            file_path = os.path.join(current_dir, filename)
            try:
                # Open in write mode and truncate
                open(file_path, "w").close()
                cleared.append(filename)
            except Exception as e:
                print(f"Could not clear {filename}: {e}")

    if cleared:
        print("Cleared contents of the following .dat files:")
        for f in cleared:
            print(f"  - {f}")
    else:
        print("No .dat files found to clear.")

if __name__ == "__main__":
    clear_dat_files()
