/* singly linked lists */

/* representation of a node */

data node {
	int val; 
	node next;	
}

/* view for a singly linked list */

ll<n> == self = null & n = 0 
	or self::node<_, q> * q::ll<n-1> 
  inv n >= 0;

sortl<n,mi,ma> ==  self::node<mi,null> & mi=ma & n=1
         or self::node<mi,q> * q::sortl<n-1,k,ma> & mi <= k
inv mi<=ma & n>0;


lseg<p, n> == self=p & n=0
	or self::node<_, q> * q::lseg<p, n-1>
	inv n>=0;

int length (node x) 
//requires x::sortl<n,mi,ma>
//ensures res = n;
requires x::sortl<n,mi,ma> & n>0
ensures x::sortl<n,mi,ma> & res = n;
{
 if (x==null) return 0;
  else if (x.next == null) return 1;
    else  {
        int r = 1+length(x.next);
        return r;
      }
}



node insert(node x, node vn)
requires x::sortl<n, mi, ma> * vn::node<v, p>
ensures  res::sortl<n+1, min(v, mi), max(v, ma)> ;

requires vn::node<v, p> & x = null
ensures  res::sortl<v, null> ;

{
  if (x==null) { vn.next=null; return vn;}
 else if (vn.val <= x.val) 
 { vn.next=x; return vn; }
  else if (x.next==null)
 { x.next=vn; vn.next=null; return x; }
  else { x.next=insert(x.next, vn); return x; }
}

node insertion_sort(node y)
requires y::ll<n> & n>0
ensures  res::sortl<n, mi, ma>;

requires y::ll<0>
ensures  res::ll<0>;
{
  if (y==null) {return y;}
  else if (y.next==null) { return y; } else {
	y.next=insertion_sort(y.next);
    return insert(y.next, y);
  }
}
 



