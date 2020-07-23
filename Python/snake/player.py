import pygame
from collections import deque
from enum import Enum, auto
from random import randint
from math import floor

class Vec:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    @staticmethod
    def random(maxx, maxy):
        return Vec(randint(0, maxx), randint(0, maxy))

    @property
    def i(self):
        return self.x
    
    @property
    def j(self):
        return self.j

    def render(self, screen, grid, color=(0, 255, 0)):
        gw, gh = grid
        w, h = screen.get_size()

        # cell size
        c_w = floor(w / gw)
        c_h = floor(h / gh)
        
        rect = pygame.Rect(self.x * c_w, self.y * c_h, c_w, c_h)
        pygame.draw.rect(screen, color, rect)

    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __mod__(self, other):
        return Vec(self.x % other.x, self.y % other.y)
    
    def __neg__(self):
        return Vec(-self.x, -self.y)

    def __repr__(self):
        return f"<{self.x}, {self.y}>"

class Snake:
    class State(Enum):
        EATEN = auto()
        ALIVE = auto()
        DEAD = auto()

    def __init__(self, grid, length):
        width, height = grid
        body = deque()

        origin = Vec.random(width - length, height)

        for i in range(length):
            body.append(origin + Vec(i, 0))
        self.body = body
        self.vel = Vec()
        self.state = Snake.State.ALIVE

    def update(self, grid, food):
        head = self.body[0]
        self.state = self.State.ALIVE

        if head == food:
            self.state = self.State.EATEN
            nhead = head + self.vel
            self.body.appendleft(nhead)
            head = self.body[0]

        if self.vel != Vec():
            head += self.vel
            self.body.pop()
            self.body.appendleft(head)
        
        width, height = grid
        # self collision
        if self.died(width, height):
            self.state = self.State.DEAD
        
        return self.state

    def render(self, screen, grid):
        d = floor(255 / len(self.body))
        for i, v in enumerate(self.body):
            v.render(screen, grid, (255 - d * i, 0, 255))

    def died(self, width, height):
        return self.body[0] in list(self.body)[1:] or self.oob(width, height)
    
    def oob(self, width, height):
        head = self.body[0]
        return head.x < 0 or head.x > width or\
            head.y < 0 or head.y > height

    def dir(self, dx, dy):
        dv = Vec(dx, dy)
        if self.vel != -dv: # No reversing
            self.vel = dv

    def __repr__(self):
        return f"Snake{self.body[0], self.vel}"
    
    def __len__(self):
        return len(self.body)