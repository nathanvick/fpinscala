def get[S]: State[S, S] = State(s => (s, s))