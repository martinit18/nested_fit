from collections import deque
from rich.text import Text as RText


class HRollingBarDisplay():
    def __init__(self, size, init_pct=0, callback=None):
        self._val_callback = callback
        self._last_val = init_pct
        self._text_size = size
        self._bar = deque(["⣀"] * self._text_size)
        self._bar_types = ["⣀", "⣤", "⣶", "⣿"]
        self._bar_colors = {
            x: ['grey', 'yellow', 'orange', 'red'][i] for i, x in enumerate(self._bar_types)
        }
        self._bar_thrs = [25.0, 50.0, 75.0]

    def __rich__(self):
        bar = RText.assemble(' ', *[(usage, self._bar_colors[usage]) for usage in self._bar])
        bar.append(
            RText.assemble(
                (f' {self._last_val:02.0f}', self._bar_colors[self._bar[-1]]),
                ('%', 'white')
            )
        )
        return bar

    def update(self, value=None):
        if value:
            self._last_val = value
        elif self._val_callback:
            self._last_val = self._val_callback()
        usage_is_max = True

        for i, thr in enumerate(self._bar_thrs):
            if self._last_val < thr:
                self._bar.append(self._bar_types[i])
                usage_is_max = False
                break

        if usage_is_max:
            self._bar.append(self._bar_types[-1])

        if len(self._bar) > self._text_size:
            self._bar.popleft()
