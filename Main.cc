
#include "cp_lib/string.cc"
#include "cp_lib/io.cc"

using namespace cp;


struct Graph {
    struct Transition {
        i32 node_index;
        str s;
        i32 minq;
        i32 maxq;
    };
    darr<darr<Transition>> transitions;

    darr<Transition>& operator[](u32 node_i);
};

darr<Graph::Transition>& Graph::operator[](u32 node_i) {
    return transitions[node_i];
}

void print(Graph::Transition t) {
    print("(", t.node_index,",", t.s, ")");
}

//init :: Graph -> darr<darr<Transition>> -> void

void init(Graph *self) {
    init(&self->transitions);
    // apply((void(*)(darr<Graph::Transition>*, u32)) &init, self->transitions);
}
void init(Graph *self, darr<darr<Graph::Transition>> tr) { 
    self->transitions = tr; 
}

//void shut(Graph *self) {
    //applyp((void(*)(darr<Graph::Transition>*)) &shut, self->transitions);
    //shut(self);
//}
void shut(Graph *self) {
    // applyp([](auto x) {shut(x);}, self->transitions);
    apply([](auto x) {shut(x);}, self->transitions);
    shut(&self->transitions);
}
u32 add_node(Graph *self, u32 count=1) {
    u32 si = len(self->transitions);
    for (u32 i = 0; i < count; i++) {
        push(&self->transitions, {});
    }
    init(&back(self->transitions));
    return si;
}

void add_transition(Graph *self, i32 node, Graph::Transition tr) {
    push(&(*self)[node], tr);
}

template <class list_t>
void cp::print(list_t list) {
    print('[');
    for (auto it = begin(list); it != end(list); it++) {
        print(*it);
        print(", ");
    }
    print(']');
}

void mabort(const char* msg) {
    perror(msg);
    exit(-1);
}

auto re_reserved = pack('?', '*', '+', '(', ')', '{', '}', '|', '\\');

Graph parse_regex(str re) {
    Graph g; init(&g);

    struct S {
        i32 i, si, fi;
    };
    darr<S> stack; init(&stack, 1);

    add_node(&g, 2);
    push(&stack, {0, 0, 1});
    while(!is_empty(stack)) {
        S& cur = back(stack);
        // cur.i, cur.si, cur.fi

        if (is_empty(re)) {
            add_transition(&g, cur.i, {cur.fi, "", 1, 1});
            pop(&stack);
            if (!is_empty(stack))
                mabort("Bad regex");

            return g;
        }

        // try process string
        if ( !is_elem(head(re), re_reserved) && (len(re) == 1 || !is_elem(re[1], re_reserved)) ) {
            str s = {re.buffer, 0};
            while (true) {
                i32 c_ind = len(s);
                if (c_ind == len(re)-1) {
                    s.cap++;
                    // u32 node_i = add_node(&g);
                    // add_transition(&g, cur.i, {node_i, s});
                    // add_transition(&g, node_i, {cur.fi, ""});
                    add_transition(&g, cur.i, {cur.fi, s, 1, 1});
                    pop(&stack);
                    if (!is_empty(stack))
                        mabort("Bad regex");
                    return g;
                }

                if (is_elem(re[c_ind+1], re_reserved)) {
                    if (is_elem(re[c_ind+1], pack('(', ')', '|'))) {
                        s.cap++;
                    }

                    // if (re[c_ind+1] == '|') {
                    //     add_transition(&g, cur.i, {(i32)cur.fi, s, 1, 1});
                    //     cur.i = cur.si;
                    //     re = drop(len(s)+1, re);
                    // } else {
                    u32 node_i = add_node(&g);
                    add_transition(&g, cur.i, {(i32)node_i, s, 1, 1});
                    cur.i = node_i;
                    re = drop(len(s), re);
                    // }
                    break;
                }

                s.cap++;
            }
            continue;
        }

        // the first sym is not reserved, but the second one is
        if ( !is_elem(head(re), re_reserved) ) {
            switch (re[1])
            {
            case '?':
                add_transition(&g, cur.i, {cur.i, str{&head(re), 1}, 0, 1});
                // push(&stack, cur);
                re = drop(2, re);
                break;
            case '*':
                add_transition(&g, cur.i, {cur.i, str{&head(re), 1}, 0, INT_MAX});
                // push(&stack, cur);
                re = drop(2, re);
                break;
            case '+':
                add_transition(&g, cur.i, {cur.i, str{&head(re), 1}, 1, INT_MAX});
                // push(&stack, cur);
                re = drop(2, re);
                break;
            case '{':
                // parse {minq, maxq}
                assert(false);
                break;

            // '|', '(', ')', or error         
            default:
                u32 node_i = add_node(&g);
                add_transition(&g, cur.i, {(i32)node_i, str{&head(re), 1}, 1, 1});
                // push(&stack, {node_i, cur.si, cur.fi});
                cur.i = node_i;
                re = drop(1, re);
                break;
            }
            continue;           
        }

        switch (head(re))
        {
        case '(': {
            i32 nsi = (i32)add_node(&g, 2);
            add_transition(&g, cur.i, {nsi, "", 1, 1});
            // push(&stack, cur);
            push(&stack, {nsi, nsi, nsi+1});
            re = drop(1, re);
        } break;

        case ')': {
            add_transition(&g, cur.i, {cur.fi, "", 1, 1});
            if (len(re) > 1) {
                switch (re[1])
                {
                case '?':
                    add_transition(&g, cur.fi, {cur.si, str{&head(re), 1}, 0, 1});
                    // push(&stack, cur);
                    re = drop(2, re);
                    break;
                case '*':
                    add_transition(&g, cur.fi, {cur.si, str{&head(re), 1}, 0, INT_MAX});
                    // push(&stack, cur);
                    re = drop(2, re);
                    break;
                case '+':
                    add_transition(&g, cur.fi, {cur.si, str{&head(re), 1}, 1, INT_MAX});
                    // push(&stack, cur);
                    re = drop(2, re);
                    break;
                case '{':
                    // parse {minq, maxq}
                    assert(false);
                    break;

                // '|', '(', ')', or error         
                default:
                    re = drop(1, re);
                    break;
                }
            }
            if (len(stack) > 1)
                stack[len(stack)-2].i = cur.fi;
            pop(&stack); // after pop cur (S&) is invalid
        } break;
        
        case '|':
            add_transition(&g, cur.i, {cur.fi, "", 1, 1});
            cur.i = cur.si;
            re = drop(1, re);
            break;
            
        
        default:
            mabort("Bad regex");
            break;
        }
    }
}

void test() {
    Graph g;
    init(&g);
    add_node(&g, 3);
    add_transition(&g, 0, {1, "a"});
    add_transition(&g, 1, {2, "b"});

    // *begin(*begin(g.transitions));
    print(g.transitions, '\n');

    
    shut(&g);
}

int main() {
    Graph g = parse_regex("abcd|a|aa");
    print(g.transitions);
}
