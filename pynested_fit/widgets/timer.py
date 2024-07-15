import time
import datetime


class NFDashboardTimer():
    def __init__(self):
        self._tp = time.time()
        self._delta = time.time() - self._tp

    def __rich__(self):
        return ' ' + self.elapsed()

    def elapsed(self):
        return str(datetime.timedelta(seconds=int(self._delta)))

    def update(self):
        self._delta = time.time() - self._tp
