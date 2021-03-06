# AUTOGENERATED! DO NOT EDIT! File to edit: nbs/ui.ipynb (unless otherwise specified).

__all__ = []

# Cell
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Perform tasks within Rural Economy project.')
    parser.add_argument('task', help='choose a task', choices=['build_parquet', 'add_rurality'])
    args = parser.parse_args()
    print(args.task, 'started')
    if args.task == 'build_parquet':
        from rurec import infogroup
        infogroup.build_parquet_dataset(21)
    elif args.task == 'add_rurality':
        from rurec import rurality
        rurality.build_parquet_dataset(11)
    print(args.task, 'finished')