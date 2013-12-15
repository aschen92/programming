# tunrstile2.py
#    Illustration of state pattern with turnstile


class Turnstile:

    def __init__(self):
        self.states = {"LOCKED": TurnstileStateLocked(self),
                       "UNLOCKED": TurnstileStateUnlocked(self)
                       }
        self.setState("LOCKED")

    def setState(self, which):
        self.state = self.states[which]

    def coin(self):
        self.state.coin()

    def enter(self):
        self.state.enter()


#----------------------------------------------------------------------
# Implementaiton of Turnstile states

class TurnstileState:

    def __init__(self, turnstile):
        self.turnstile = turnstile


class TurnstileStateLocked(TurnstileState):

    def coin(self):
        print("Locked state: Coin Accepted")
        self.turnstile.setState("UNLOCKED")

    def enter(self):
        print("Locked state: Attempt to Enter, SOUND ALARM")


class TurnstileStateUnlocked(TurnstileState):

    def coin(self):
        print("Unlocked state: Coin entered, return it")

    def enter(self):
        print("Unlocked state: Passenger enters")
        self.turnstile.setState("LOCKED")

                              
