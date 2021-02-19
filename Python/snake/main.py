#!/bin/envrun
import sys, pygame
from player import Vec, Snake
pygame.init()

SIZE = 600, 600
GRID = width, height = 30, 30
black = 0, 0, 0
white = 255, 255, 255

screen = pygame.display.set_mode(SIZE)

SLEN = 5
FRATE = 15
snake = Snake(GRID, SLEN)
food = Vec.random(*GRID)

run = False
clk = pygame.time.Clock()
pygame.font.init()
font = pygame.font.Font(pygame.font.get_default_font(), 18)
while 1:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            pressed = pygame.key.get_pressed()
            if pressed[pygame.K_q]:
                sys.exit()
            elif pressed[pygame.K_a] or pressed[pygame.K_LEFT]:
                snake.dir(-1, 0)
            elif pressed[pygame.K_d] or pressed[pygame.K_RIGHT]:
                snake.dir(1, 0)
            elif pressed[pygame.K_w] or pressed[pygame.K_UP]:
                snake.dir(0, -1)
            elif pressed[pygame.K_s] or pressed[pygame.K_DOWN]:
                snake.dir(0, 1)

    clk.tick_busy_loop(FRATE)
    screen.fill(black)

    state = snake.update(GRID, food)

    scr = f"SCORED: {len(snake) - SLEN}"
    if state == Snake.State.DEAD:
        print(scr)
        snake = Snake(GRID, SLEN)
    elif state == Snake.State.EATEN:
        food = Vec.random(*GRID)

    snake.render(screen, GRID)
    food.render(screen, GRID)

    text_surface = font.render(scr, True, white)
    screen.blit(text_surface, dest=(0,0))
    pygame.display.flip()
    run = True