import bigRat from "big-rational";
import bigInt from "big-integer";

bigRat.min = (q1, q2) => (q1.leq(q2) ? q1 : q2);
bigRat.max = (q1, q2) => (q1.geq(q2) ? q1 : q2);
bigRat.clip = (q, qmin, qmax) => bigRat.max(qmin, bigRat.min(q, qmax));

