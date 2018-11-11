import pygame
import math
import colorsys

pygame.init()
WIDTH=pygame.display.Info().current_w
#WIDTH=700
HEIGHT=pygame.display.Info().current_h
#HEIGHT=400
RHEIGHT=HEIGHT/10
RATE = 10
FONTSPACING=15
FONTSIZE=30
FONTRATE=12
PADDING=10
LOGORATE=7
COLORPHASE = 10

title = 'Another Quality Release by'
title = title.upper().replace('I','i')
gamename = '-- VentCrawler --'
message = 'Hot off the vim and GHC consoles of grm, B$, and joshy comes VentCrawler: the dank new functional rougelike. It isn\'t very good, but at least it got this cool intro. Features include: walking stairs, 5% compilation rate, and a handheld game-breaking drill. Press enter to continue to the actual game. shoutouts to simpleflips.'
message = message.upper().replace('I','i')
screen = pygame.display.set_mode((WIDTH, HEIGHT), pygame.FULLSCREEN)
#screen = pygame.display.set_mode((WIDTH, HEIGHT))

logo = pygame.transform.scale(pygame.image.load('cosi.png'), (int(1.13333*HEIGHT/3), int(HEIGHT/3)))

font = pygame.font.Font('dos.ttf', FONTSIZE)

pygame.mixer.music.load('ec-druid.mod')
loop_event = pygame.USEREVENT + 1
pygame.mixer.music.set_endevent(loop_event)
pygame.mixer.music.play()

clock = pygame.time.Clock()
tl = 0

ticktock = 0
done = False

while not done:
    for event in pygame.event.get():
        if event.type == pygame.KEYDOWN and event.key == pygame.K_RETURN:
            done = True
        if event.type == pygame.QUIT:
            done = True
        if event.type == loop_event:
            pygame.mixer.music.play()

    screen.fill((0,0,0))

    ticktock += 1
    y1 = ((math.sin(ticktock/(RATE*math.pi))+1)/2)*(HEIGHT-RHEIGHT)
    y2 = ((math.sin(ticktock/(RATE*math.pi) + (2*math.pi/3))+1)/2)*(HEIGHT-RHEIGHT)
    y3 = ((math.sin(ticktock/(RATE*math.pi) + (4*math.pi/3))+1)/2)*(HEIGHT-RHEIGHT)

    z1 = (ticktock/(RATE*math.pi) + (1*math.pi/3)) % (2*math.pi)
    z2 = (ticktock/(RATE*math.pi) + (3*math.pi/3)) % (2*math.pi)
    z3 = (ticktock/(RATE*math.pi) + (5*math.pi/3)) % (2*math.pi)

    c1 = (((math.sin(ticktock/(RATE*math.pi)+2*math.pi/3)+1)/2*255),((math.sin(ticktock/(RATE*math.pi)+4*math.pi/3)+1)/2*255),((math.sin(ticktock/(RATE*math.pi))+1)/2*255))
    c3 = (((math.sin(ticktock/(RATE*math.pi)+4*math.pi/3)+1)/2*255),((math.sin(ticktock/(RATE*math.pi))+1)/2*255),((math.sin(ticktock/(RATE*math.pi)+2*math.pi/3)+1)/2*255))
    c2 = (((math.sin(ticktock/(RATE*math.pi))+1)/2*255),((math.sin(ticktock/(RATE*math.pi)+2*math.pi/3)+1)/2*255),((math.sin(ticktock/(RATE*math.pi)+4*math.pi/3)+1)/2*255))

    zzz = sorted(zip([y1, y2, y3], [z1, z2, z3], [c1,c2,c3]), key=lambda pair: pair[1], reverse=True)
    
    pygame.draw.rect(screen, zzz[0][2], pygame.Rect(0,zzz[0][0], WIDTH, RHEIGHT))
    pygame.draw.rect(screen, zzz[1][2], pygame.Rect(0,zzz[1][0], WIDTH, RHEIGHT))
    pygame.draw.rect(screen, zzz[2][2], pygame.Rect(0,zzz[2][0], WIDTH, RHEIGHT))
   
    cl = 0
    cc = 0
    for c in title:
        cl += FONTSPACING
        cc += COLORPHASE
        hsv = colorsys.hsv_to_rgb(((ticktock+cc)%360)/360.0,1,1)
        textsurface = font.render(c, False, (255*hsv[0],255*hsv[1],255*hsv[2]))
        screen.blit(textsurface, (WIDTH/2-FONTSIZE*len(title)/4+cl,HEIGHT/20))

    cl = 0
    for c in message:
        cl += FONTSPACING
        textsurface = font.render(c, False, (255,255,255))
        screen.blit(textsurface, ((WIDTH-tl)+cl, 3*HEIGHT/4+HEIGHT/8*(math.cos(ticktock/(FONTRATE*math.pi)+cl/(FONTSPACING*math.pi)))/2))

    tl += 2
    if tl >= WIDTH+FONTSPACING*(len(message)+PADDING):
        tl = 0

    textsurface = font.render(gamename, False, (255,255,255))
    screen.blit(textsurface, (WIDTH/2-FONTSIZE*len(gamename)/4,12*HEIGHT/20))

    screen.blit(pygame.transform.rotate(logo,ticktock%360),(WIDTH/2+WIDTH/4*math.sin(ticktock/(LOGORATE*math.pi))-int(1.13333*HEIGHT/3)/2,HEIGHT/10))
    pygame.display.flip()

    clock.tick(60)

pygame.display.quit()
pygame.quit()
