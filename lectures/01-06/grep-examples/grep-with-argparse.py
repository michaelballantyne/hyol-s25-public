import argparse

def parse_args():
    parser = argparse.ArgumentParser(description="A grep-like tool with filtering options.")

    parser.add_argument("pattern", help="The search pattern (regex or string).")
    parser.add_argument("path", help="The path to search in.")

    parser.add_argument(
        "--include", 
        action="append", 
        dest="filters", 
        metavar="pattern",
        help="pattern to include"
    )

    parser.add_argument(
        "--exclude", 
        action="append", 
        dest="filters", 
        metavar="pattern",
        help="pattern to exclude"
    )

    parser.add_argument(
        "-c", 
        action="store_true", 
        help="Only print a count of matching lines per file."
    )

    parser.add_argument(
        "-r", 
        action="store_true", 
        help="Recursively search directories."
    )

    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    print(args)