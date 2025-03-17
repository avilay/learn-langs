import numpy as np
from tqdm import tqdm
import time

rng = np.random.default_rng()


def main():
    total_work = 50 * 4
    progress_bar = tqdm(total=total_work)
    work_done = 0
    for _ in range(50):
        inc_work = rng.integers(1, 5)
        time.sleep(0.5)
        progress_bar.update(inc_work)
        work_done += inc_work
    progress_bar.update(total_work - work_done)
    progress_bar.close()


if __name__ == "__main__":
    main()
