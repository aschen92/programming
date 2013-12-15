# pizza.py
# Aaron Schendel


class Pizza:

    def __init__(self):
        self.states = {"PREPARED": PizzaStatePrepared(self),
                       "BAKED": PizzaStateBaked(self),
                       "DELIVERED": PizzaStateDelivered(self)
                       }

    def setState(self, which):
        self.state = self.states[which]

    def bake(self):
        self.state.bake()

    def deliver(self):
        self.state.deliver()



#-------------------------------------------------------------------
# Implementation of Pizza states

class PizzaState:

    def __init__(self, pizza):
        self.pizza = pizza


class PizzaStatePrepared(PizzaState):

    def bake(self):
        self.pizza.setState("BAKED")
        print("Pizza finished baking!")

    def deliver(self):
        print("ERROR: Cannot deliver an unprepared pizza!")


class PizzaStateBaked(PizzaState):

    def bake(self):
        print("ERROR: Cannot bake a pizza that is already baked!")

    def deliver(self):
        self.pizza.setState("BAKED")
        print("Pizza has been delivered!")

class PizzaStateDelivered(PizzaState):

    def bake(self):
        print("ERROR: Cannot bake a pizza that has already been delivered!")

    def deliver(self):
        print("ERROR: Cannot deliver a pizza twice!")
