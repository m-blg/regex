
#include "cp_lib/buffer.cc"
#include "cp_lib/string.cc"
#include "cp_lib/io.cc"
#include "cp_lib/basic.cc"
#include <cassert>
#include <cstdio>
#include <cstring>

using namespace cp;


struct Regex_FA {
    struct Transition {
        i32 node_index;
        dstrb s;
        i32 minq;
        i32 maxq;
        i32 q;
    };
    darr<darr<Transition>> transitions;

    darr<Transition>& operator[](u32 node_i);
};

darr<Regex_FA::Transition>& Regex_FA::operator[](u32 node_i) {
    return transitions[node_i];
}

void print(Regex_FA::Transition t) {
    print("(", t.node_index,",", t.s, ")");
}

dstrb to_graphvis(Regex_FA g) {
    dstrb sb; init(&sb, 30 * len(g.transitions));

    sprint_fmt(&sb, "digraph G {");
    for (u32 i = 0; i < len(g.transitions); i++) {
        for (u32 j = 0; j < len(g[i]); j++) {
            str s = to_str(g[i][j].s);
            if (is_empty(s))
                s = str{"\\\\e"};
            sprint_fmt(&sb, "\t%d -> %d [label=\"%.*s, %d, %d\"];\n", 
                    i, g[i][j].node_index, len(s), s.buffer, 
                    g[i][j].minq, g[i][j].maxq);
        }
    }
    sprint_fmt(&sb, "}");

    return sb;
}

//init :: Regex_FA -> darr<darr<Transition>> -> void

void init(Regex_FA *self) {
    init(&self->transitions);
    // apply((void(*)(darr<Regex_FA::Transition>*, u32)) &init, self->transitions);
}
void init(Regex_FA *self, darr<darr<Regex_FA::Transition>> tr) { 
    self->transitions = tr; 
}

//void shut(Regex_FA *self) {
    //applyp((void(*)(darr<Regex_FA::Transition>*)) &shut, self->transitions);
    //shut(self);
//}
void shut(Regex_FA *self) {
    // applyp([](auto x) {shut(x);}, self->transitions);
    apply([](auto x) {shut(x);}, self->transitions);
    shut(&self->transitions);
}
u32 add_node(Regex_FA *self, u32 count=1) {
    u32 si = len(self->transitions);
    for (u32 i = 0; i < count; i++) {
        push(&self->transitions, {});
    }
    init(&back(self->transitions));
    return si;
}

void add_transition(Regex_FA *self, i32 node, Regex_FA::Transition tr) {
    push(&(*self)[node], tr);
}


void mabort(const char* msg) {
    perror(msg);
    exit(-1);
}

auto re_reserved = pack('?', '*', '+', '(', ')', '{', '}', '|');
auto re_reserved_group = pack('(', ')', '|');
auto re_reserved_quantifier = pack('?', '*', '+', '{');


char
parse_letter(str *re) {
    if ((*re)[0] == '\\') {
        if (len(*re) == 1)
            mabort("bad regex");
        char l = (*re)[1];
        *re = drop(2, *re);
        return l;
    }
    char l = (*re)[0];
    *re = drop(1, *re);
    return l;
}

// doesn't include the letter before a quantifier
dstrb
parse_string(str *re) {
    //if (is_empty(*re) || is_elem((*re)[0], re_reserved))
        //return {};

    dstrb s; init(&s, cap(*re));
    while(!is_empty(*re)) {

        if (is_elem((*re)[0], re_reserved)) {
            if (!is_elem((*re)[0], re_reserved_group)) {
                s.len--;
                re->buffer--;
                re->cap++;
            }
            break;
        }
        char l = parse_letter(re);
        push(&s, l);
    }

    return s;
}

void
parse_quantifier(str *re, i32 *minq, i32 *maxq) {
    switch (head(*re)) {
    case '?': {
        *re = drop(1, *re);
        *minq = 0; *maxq = 1;
    } break;

    case '*': {
        *re = drop(1, *re);
        *minq = 0; *maxq = INT_MAX;
    } break;

    case '+': {
        *re = drop(1, *re);
        *minq = 1; *maxq = INT_MAX;
    } break;

    case '{': {
        const char* p = strchr(re->buffer, '}');
        if (p == null)
            mabort("Bad regex");
        i32 l = p - re->buffer + 1;
                
        if (sscanf(re->buffer, "{%d, %d}", minq, maxq) != 2)
            mabort("Bad regex");
        *re = drop(l, *re);
    } break;

    default: {
         mabort("Bad regex");
    } break;
    }
}

dstrb
parse_letter_with_quantifier(str *re, i32 *minq, i32 *maxq) {
    dstrb s; init(&s, 1);
    push(&s, parse_letter(re));
    parse_quantifier(re, minq, maxq);
    return s;
}

void
parse_group_quantifier(str *re, i32 *minq, i32 *maxq) {
    *re = drop(1, *re);
    i32 bq = 1;
    while (bq > 0 && !is_empty(*re)) {
        if (head(*re) == '(') {    bq++; } 
        else if (head(*re) == ')') bq--;
        *re = drop(1, *re);
    }
    if (bq != 0)
        mabort("Bad regex");

    if (is_empty(*re) || !is_elem(head(*re), re_reserved_quantifier)) {
        *minq = 1, *maxq = 1;
        return;
    }
    parse_quantifier(re, minq, maxq);
}

Regex_FA
parse_regex(str re)
{
    Regex_FA g; init(&g);

    struct S {
        i32 i, si, fi;
        str re;
    };
    darr<S> stack; init(&stack, 1);

    add_node(&g, 2);
    push(&stack, {0, 0, 1, re});
    while(!is_empty(stack)) {
        S& cur = back(stack);
        // cur.i, cur.si, cur.fi

        if (is_empty(cur.re)) {
            add_transition(&g, cur.i, {cur.fi, dstrb_from(""), -1, 1});
            pop(&stack);
            if (!is_empty(stack))
                mabort("Bad regex");

            return g;
        }

        if (!is_elem(cur.re[0], re_reserved)) {
            dstrb s = parse_string(&cur.re);
            if (!is_empty(s)) {
                //if (!is_empty(re) && (re[0] == '|' || re[0] == ')'))
                i32 node_i = (i32)add_node(&g);
                add_transition(&g, cur.i, {node_i, s, -1, 1});
                cur.i = node_i;
                continue;
            }

            i32 minq, maxq;
            dstrb l = parse_letter_with_quantifier(&cur.re, &minq, &maxq);
            i32 node_i = (i32)add_node(&g);
            add_transition(&g, cur.i, {node_i, dstrb_from(""), -1, 1});
            add_transition(&g, node_i, {node_i, l, minq, maxq});
            cur.i = node_i;
            continue;
        }

        switch (head(cur.re)) 
        {
        case '(': {
            i32 minq, maxq;
            str temp_re = drop(1, cur.re);
            parse_group_quantifier(&cur.re, &minq, &maxq);

            i32 loop_node_i = (i32)add_node(&g, 2);
            i32 sni = loop_node_i + 1;
            add_transition(&g, cur.i, {loop_node_i, dstrb_from(""), -1, 1}); // from cur to loop node
            add_transition(&g, loop_node_i, {sni, dstrb_from(""), minq, maxq}); // from loop node to group start node
            cur.i = loop_node_i;
            push(&stack, {sni, sni, loop_node_i, temp_re});
        } break;

        case ')': {
            add_transition(&g, cur.i, {cur.fi, dstrb_from(""), -1, 1});
            pop(&stack);
        } break;

        case '|': {
            add_transition(&g, cur.i, {cur.fi, dstrb_from(""), -1, 1});
            cur.i = cur.si;
            cur.re = drop(1, cur.re);
        } break;

        default: {
            mabort("Bad regex");
        } break;
        }

    }

    mabort("Bad regex");
    return g;
}

bool
are_transitions_correct(Regex_FA re) {
    for (auto it = begin(re.transitions); it != end(re.transitions); it++) {
        for (auto it2 = begin(*it); it2 != end(*it); it2++) {
            if (it2 != begin(*it) && it2->minq > 0)
                return false;
        }
    }

    return true;
}

void
test1() {
    Regex_FA g;
    init(&g);
    add_node(&g, 3);
    add_transition(&g, 0, {1, dstrb_from("a")});
    add_transition(&g, 1, {2, dstrb_from("b")});

    // *begin(*begin(g.transitions));
    print(g.transitions, '\n');

    
    shut(&g);
}

void
test_regex(str re, str of_name_no_ext) {
    Regex_FA g = parse_regex(re);
    printf("Are transitions correct: %d\n", are_transitions_correct(g));

    // print(g.transitions);
    dstrb gvis = to_graphvis(g);

    dstrb of_dot; init(&of_dot, len(of_name_no_ext) + 4);
    dstrb of_png; init(&of_png, len(of_name_no_ext) + 4);
    cat(&of_dot, pack(of_name_no_ext, str{".dot\0"}));
    cat(&of_png, pack(of_name_no_ext, str{".png\0"}));

    write(to_str(gvis), of_dot.buffer);
    dstrb cmd; init(&cmd, 30);
    sprint_fmt(&cmd, "dot -Tpng %s -o %s\0", of_dot.buffer, of_png.buffer);
    system(cmd.buffer);
    clear(&cmd); sprint_fmt(&cmd, "rm %s\0", of_dot.buffer);
    system(cmd.buffer);

    shut(&gvis);
    shut(&of_dot);
    shut(&of_png);
    shut(&cmd);

    shut(&g);
}


// NOTE: we need explicit loop flags (let's use q field of Transition)

bool
match(Regex_FA re, str s)
{
    struct S {
        i32 state_i, tran_i;
        str s;
    };
    darr<S> stack; init(&stack, 1);

    push(&stack, {0, 0, s});

    while (!is_empty(stack)) {
        S cur = back(stack);
        pop(&stack);

        if (cur.state_i == 1) { // finish state
            return is_empty(cur.s);
        } 
        
        assert(is_bounded(cur.state_i, 0, (i32)len(re.transitions) - 1));
        assert(is_bounded(cur.tran_i, 0, (i32)len(re.transitions[cur.state_i]) - 1));
        // if (!is_bounded(cur.tran_i, 0, len(re.transitions[cur.state_i]) - 1)) {
        //     pop(&stack);
        //     continue;
        // }
        Regex_FA::Transition& tran = re.transitions[cur.state_i][cur.tran_i];
        if (tran.q >= tran.maxq) { // end of the loop
            tran.q = 0;
            cur.tran_i++;
            push(&stack, cur);
            continue;
        }
        if (tran.q >= tran.minq && cur.tran_i < len(re.transitions[cur.state_i]) - 1) {
            push(&stack, {cur.state_i, cur.tran_i+1, cur.s});
        }
        
        if (to_str(tran.s) != take(len(tran.s), cur.s)) {
            if (cur.tran_i == len(re.transitions[cur.state_i]) - 1 ||
                tran.q < tran.minq)
            {
                // fail: whole state, backtrack
                
                if (is_empty(stack))
                    return false;
                S& frame = back(stack);
                re[frame.state_i][0].q = 0; // reset counter for loop (if present)
                continue;
            }
                
            if (cur.tran_i == 0) // if loop (or not)
                re[cur.state_i][0].q = 0; 
            cur.tran_i++;

            push(&stack, cur);
            continue;
        }

        cur.s = drop(len(tran.s), cur.s);
        if (cur.tran_i == 0 && tran.minq != -1) // if loop
            tran.q++;
        cur.state_i = tran.node_index;  
        cur.tran_i = 0;
        push(&stack, cur);
    }

    assert(false);
    return false;
}


void
test_match(str re, str s) {
    Regex_FA re_fa = parse_regex(re);
    printf("Result: %d\n", match(re_fa, s));
}    

int main() {
    // test_regex("(c|b){3,4}", "test/test1");
    // test_regex("((o|b)+|c{3, 4}){9, 11}", "test/test2");
    // test_match("ab+a", "abbbbbbbbbbba");
    // test_regex("a(b|c)a", "test/test3");
    // test_match("a(b|c)a", "aba");
    // test_regex("a(b|c)*a", "test/test4");
    test_match("a(b|c)*a", "abcba");
    // test_match("(b|c)", "c");
}
