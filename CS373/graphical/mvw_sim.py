# mvw_sim.py
import time, random

from graphics import *
from pylogic import KB

RULES = dict(aux_battery=True,
             furniture_prob=20,
             dirt_prob=25,
             charger_prob=5,
             full_capacity=10,
             cleanup_score=20,
             deposit_score=10,
             full_charge=40)

ACTIONS = "right left forward suck dump charge".split()

DIR_XY = dict(north=(0,-1), east=(1,0), south=(0,1), west=(-1,0))
DIR_LEFT = dict(north='west', east='north', south='east', west='south')
DIR_RIGHT = dict(west='north', north='east', east='south', south='west')

#---------------------------------------------------------------------------
# functions for translating locations into Prolog notation
def proLoc(Loc):
    x,y = Loc
    return "xy({:d},{:d})".format(x+1,y+1) # indexes on Prolog side are 1 based

def proLocList(lst):
    return "["+",".join([proLoc(p) for p in lst])+"]"

#---------------------------------------------------------------------------
class Agent:

    def __init__(self, i, size):
        self.index = i
        self.name = "Agent {:d}".format(i)
        self.charge = RULES["full_charge"]
        self.capacity = RULES["full_capacity"]
        self.dir = "east" if i == 1 else "west"
        self.pos = (0,size-1) if i == 1 else (size-1,0)
        self.home = self.pos
        self.last_action = None
        self.actions = 0
        self.score = 0
        
    def prologPairs(self):
        return ["charge({:d})- {:d}".format(self.index,self.charge),
                "capacity({:d})- {:d}".format(self.index,self.capacity),
                "dir({:d})-{:s}".format(self.index,self.dir),
                "pos({:d})-{:s}".format(self.index,proLoc(self.pos)),
                "home({:d})-{:s}".format(self.index,proLoc(self.home))
                ]
                
class WorldState:

    def __init__(self,  size, dirt, furniture, chargers):
        self.agents = [Agent(1,size), Agent(2,size)]
        self.size = size
        self.dirt = dirt
        self.furniture = furniture
        self.chargers = chargers

    def prologState(self):
        pairs = (self.agents[0].prologPairs() +
                 self.agents[1].prologPairs() +
                 ["size-{:d}".format(self.size),
                  "dirt-{:s}".format(proLocList(self.dirt)),
                  "furniture-{:s}".format(proLocList(self.furniture)),
                  "chargers-{:s}".format(proLocList(self.chargers)),
                  "max_charge-{:d}".format(RULES["full_charge"]),
                  "max_capacity-{:d}".format(RULES["full_capacity"]),
                  ]
                 )
        return "["+",".join(pairs)+"]"
                                        
    def setNames(self, names):
        for agent, n in zip(self.agents,names):
            agent.name = n

    def pointsLeft(self):
        ans = False
        for agent in self.agents:
            if ((agent.charge > 0 or RULES["aux_battery"]) and
                (self.dirt != [] or agent.capacity < RULES["full_capacity"])):
                ans = True
        return ans
                        
    def update(self, action, i):
        if action in ACTIONS:
            agent = self.agents[i]
            if self.can_act(agent):
                method = getattr(self, "do_"+action)
                method(agent)
        if agent.charge > 0:
            agent.charge -= 1
        agent.last_action = action
        agent.actions += 1

    def can_act(self, agent):
        return agent.charge > 0 or RULES["aux_battery"]

    def inBounds(self, loc):
        x,y = loc
        return 0 <= x < self.size and 0 <= y < self.size

    def isOpen(self, loc):
        return (loc not in self.furniture and
                loc != self.agents[0].pos and
                loc != self.agents[1].pos)

    def getScores(self):
        scores = [agt.score for agt in self.agents]
        return scores
            
    def do_right(self, agent):
        agent.dir = DIR_RIGHT[agent.dir]

    def do_left(self, agent):
        agent.dir = DIR_LEFT[agent.dir]

    def do_forward(self, agent):
        x,y = agent.pos
        dx,dy = DIR_XY[agent.dir]
        loc = x+dx, y+dy
        if self.inBounds(loc) and self.isOpen(loc):
            agent.pos = loc

    def do_dump(self, agent):
        if agent.pos == agent.home:
            load = RULES["full_capacity"] - agent.capacity
            agent.score += RULES["deposit_score"] * load
            agent.capacity = RULES["full_capacity"]

    def do_suck(self, agent):
        if agent.charge > 0:
            if agent.pos in self.dirt:
                agent.score += RULES["cleanup_score"]
                self.dirt.remove(agent.pos)
                agent.capacity -= 1

    def do_charge(self, agent):
        #print "CHARGING", agent.index
        #print agent.pos, agent.home, self.chargers
        if agent.pos == agent.home or (agent.pos in self.chargers):
            agent.charge = RULES["full_charge"]
            #print "CHARGE DONE"

class VacStateWin:

    # VacStateWin shows a view of a state. Just hand it a state, and
    #   a window will pop up.

    VacSize = .2
    VacPoints = {'north':(0,VacSize,0,-VacSize), 'east':(-VacSize,0,VacSize,0),
                 'south':(0,-VacSize,0,VacSize), 'west':(VacSize,0,-VacSize,0)}

    color1 = "black"
    color2 = "orange"

    agentInfo = "{name:s}\n\nScore:   {score:3d}\nCharge:  {charge:3d}\nCapacity:{capacity:3d}\nActions: {actions:3d}\nLast:   {last_action:s}"
    

    def __init__(self, state, timeLimit, height=800, title="Vacuum World"):
        xySize = state.size
        win = self.win = GraphWin(title, 1.33*height, height, autoflush=False)
        win.setBackground("gray90")
        win.setCoords(-0.6, xySize-.4, 1.33*xySize-.5, -0.6)
        cells = self.cells = {}
        for x in range(xySize):
            for y in range(xySize):
                cells[(x,y)] = Rectangle(Point(x-.5,y-.5),Point(x+.5,y+.5))
                cells[(x,y)].setWidth(2)
                cells[(x,y)].draw(win)
        self.vacs = []
        for loc,cell in self.cells.items():
            if loc in state.chargers:
                p1 = cell.getP1()
                p2 = cell.getP2()
                p1.move(.05,.05)
                p2.move(-.05,-.05)
                highlight = Rectangle(p1,p2)
                highlight.setWidth(3)
                highlight.setOutline("yellow2")
                highlight.draw(win)

        ccenter = 1.167*(xySize-.5)
        self.time = Text(Point(ccenter, (xySize-1)*.5 ), "")
        self.time.setSize(36)
        self.time.setFill("red")
        self.time.draw(win)

        self.agent1 = Text(Point(ccenter, (xySize-1)*(1-.125)), "")
        self.agent1.setSize(18)
        self.agent1.setFill(self.color1)
        self.agent1.setFace("courier")
        self.agent1.setStyle("bold")
        self.agent1.draw(win)
        
        self.agent2 = Text(Point(ccenter, (xySize-1)*.125),"")
        self.agent2.setSize(18)
        self.agent2.setFill(self.color2)
        self.agent2.setFace("courier")
        self.agent2.setStyle("bold")
        self.agent2.draw(win)
                
        self.update(state, timeLimit)


    def update(self, state, timeLeft):
        # View state in exiting window
        for loc,cell in self.cells.items():
            if loc in state.dirt:
                cell.setFill("gray")
            elif loc in state.furniture:
                cell.setFill("black")
            else:
                cell.setFill("white")

        if self.vacs:
            for v in self.vacs:
                v.undraw()
            self.vacs = []

        for a in state.agents:
            x,y = a.pos
            dx0, dy0, dx1, dy1 = self.VacPoints[a.dir]
            p1 = Point(x+dx0,y+dy0)
            p2 = Point(x+dx1,y+dy1)
            vac = Line(p1, p2)
            vac.setWidth(5)
            vac.setArrow('last')
            if a.index == 1:
                vac.setFill(self.color1)
            else:
                vac.setFill(self.color2)
            vac.draw(self.win)
            self.vacs.append(vac)
            self.displayAgentInfo(a)

        minutes = timeLeft // 60
        seconds = timeLeft % 60
        self.time.setText("{:2d}:{:02d}".format(minutes,seconds))

        self.win.flush()

    def displayAgentInfo(self, a):
        if a.index == 1:
            infoText = self.agent1
        else:
            infoText = self.agent2
        lines = self.agentInfo.format(**a.__dict__)
        ljustLines = [x.ljust(15) for x in lines.split("\n")]
        infoText.setText("\n".join(ljustLines))
        

    def pause(self):
        self.win.getMouse()


class Simulation:

    def __init__(self, strategies, state, timeLimit, size=750):
        # create prolog processes and tell them their agent #
        self.strategies = [KB(strat) for strat in strategies]
        for i, kb in enumerate(self.strategies):
            kb.ask("set_id({:d})".format(i+1))

        self.state = state
        state.setNames(strategies)
        self.win = VacStateWin(state, timeLimit, size)
        self.timeLimit = timeLimit
        self.iter_time = None

    def elapsed(self):
        return int(time.time()-self.startTime)

    def rate(self, n):
        now = time.time()
        recip = 1.0/n
        if self.iter_time is None:
            time.sleep(recip)
        else:
            elapsed = now - self.iter_time 
            tosleep = max(recip - elapsed, 0)
            time.sleep(tosleep)
        self.iter_time = time.time()
            

    def run(self):
        self.win.pause()
        self.startTime = time.time()
        # send initial action requests
        for i in range(len(self.strategies)):
            self.requestNextAction(i)
        while self.elapsed() < self.timeLimit and self.state.pointsLeft():
            self.rate(15)
            actions = self.getActions()
            for agentNo, action in actions:
                print agentNo, action
                #if action not in ["left","right","forward"]:
                    #self.win.pause()
                self.state.update(action, agentNo)
                self.requestNextAction(agentNo)
            self.win.update(self.state, self.timeLimit-self.elapsed())
            #time.sleep(.05)
            #self.win.pause()
        self.win.update(self.state, self.timeLimit-self.elapsed())
        self.win.pause()

    def getActions(self):
        actions = []
        for i,kb in enumerate(self.strategies):
            resp = None
            if kb.response_ready():
                try:
                    resp = kb.getResponse()
                except:
                    print "ERROR in response from agent", i
                if resp:
                    actions.append((i,resp["Action"]))
                else:
                    print "ERROR got None from agent", i
        random.shuffle(actions)
        return actions

    def requestNextAction(self, agentNo):
        kb = self.strategies[agentNo]
        percept = self.state.prologState()
        #print percept
        query = "act({:s},Action)".format(percept)
        kb.ask_nowait(query)

    def getScores(self):
        return self.state.getScores()
                
                
def RandState(n, dprob=0.25, fprob=0.20, cprob=.05):
    # Returns a random start state.
    locations = [(x,y) for x in range(n) for y in range(n)]
    # remove home locations as valid places for dirt, furnitured, etc
    locations.remove((0,n-1))
    locations.remove((n-1,0))
    furniture = [loc for loc in locations[1:] if random.random() < fprob]
    dirt = [loc for loc in locations
                  if random.random() < fprob and loc not in furniture]
    chargers = [loc for loc in locations
                  if random.random() < cprob and loc not in furniture]
    return WorldState(n, dirt, furniture, chargers)



if __name__ == "__main__":
    import sys
    if len(sys.argv) != 5:
        print "Usage: mvw_sim.py env_size time_limit agt1 agt2"
        exit(1)
    state = RandState(int(sys.argv[1]))
    sim = Simulation([sys.argv[3],sys.argv[4]], state, int(sys.argv[2]), 750)
    sim.run()
    print sim.getScores()
