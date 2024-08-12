# Brief  : Creates a richs text object that can 
#          be updated without recreating its parent.
# Author : César Godinho
# Date   : 12/08/2024

class RTRow():
    def __init__(self, text: str = ''):
        self._text = text

    def __rich__(self):
        return self._text

    def update(self, text: str) -> None:
        self._text = text
