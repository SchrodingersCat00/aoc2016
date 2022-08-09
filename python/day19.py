from typing import List
class Table:
    def __init__(self, size) -> None:
        self.data = [*range(1, size), 0]

    def get_neighbour(self, elve: int) -> int:
        return self.data[elve]
    
    def take_from_neighbor(self, elve: int):
        self.data[elve] = self.data[self.data[elve]]

def do(table: Table):
    i = 0
    while table.get_neighbour(i) != i:
        # print(f'{i} takes from {table.get_neighbour(i)}')
        table.take_from_neighbor(i)
        i = table.get_neighbour(i)

    print(i+1)
        

def main():
    do(Table(3018458))

if __name__ == '__main__':
    main()