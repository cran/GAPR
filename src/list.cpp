#include "list.h"

#ifdef __cplusplus
extern "C"
{
#endif

int* ivector1_2(int length)
{
  int *out;
  out=new int[length];
  out--;
  return out;
}


//

gInt::gInt(int _data)
{
  data=_data;
}

//
// ListNode
//

ListNode::ListNode()
{
  _val=NULL;
  _next=_prev=this;
}

ListNode::ListNode(gObject *val)
{
  _val=val;
  _next=_prev=this;
}

ListNode::~ListNode()
{
}

ListNode *ListNode::next()
{
  return _next;
}

ListNode *ListNode::prev()
{
  return _prev;
}

ListNode *ListNode::insert(ListNode *b)
{
  ListNode *c=_next;
  b->_next=c;
  b->_prev=this;
  _next=b;
  c->_prev=b;
  return b;
}

ListNode *ListNode::remove()
{
  _prev->_next=_next;
  _next->_prev=_prev;
  _next=_prev=this;
  return this;
}

void ListNode::splice(ListNode *b)
{
  ListNode *a=this;
  ListNode *an=a->_next;
  ListNode *bn=b->_next;
  a->_next=bn;
  b->_next=an;
  an->_prev=b;
  bn->_prev=a;
}

//
// List
//

List::List()
{
  _index=0;
  _length=0;
  header=new ListNode();
  win=header;
}

List::~List()
{
  while(length() > 0){
    first();
    delete remove();
  }
  delete header;
}

gObject* List::insert(gObject *val)
{
  win->insert(new ListNode(val));
  ++_length;
  return val;
}

gObject* List::prepend(gObject *val)
{
  header->insert(new ListNode(val));
  ++_length;
  if(_index>0) _index++;
  return val;
}

gObject* List::append(gObject *val)
{
  header->prev()->insert(new ListNode(val));
  ++_length;
  return val;
}

List* List::append(List *l)
{
  ListNode *a=header->prev();
  a->splice(l->header);
  _length+=l->_length;
  l->header->remove();
  l->_length=0;
  l->win=l->header;
  return this;
}

gObject* List::remove()
{
  if(win == header) return NULL;
  gObject *val=win->_val;
  win=win->prev();
  delete win->next()->remove();
  --_length;
  --_index;
  return val;
}

gObject* List::val(gObject *val)
{
  gObject *oval;
  oval=NULL;
  if(win != header){
    oval=win->_val;
    win->_val=val;
  }
  return oval;
}

gObject* List::val()
{
  return win->_val;
}

gObject* List::GetAt(int _pt)
{
  if((_pt < 1) || (_pt > _length)) return NULL;
  int i;
        if(_pt < _index){
    for(i=_index;i>_pt;i--)
      win=win->_prev;
    _index=_pt;
    return win->_val;
  } else if(_pt > _index){
    for(i=_index;i<_pt;i++)
      win=win->_next;
    _index=_pt;
    return win->_val;
  } else return win->_val;
}

gObject* List::next()
{
  win=win->next();
  _index++;
  if(_index>_length) _index=0;
  return win->_val;
}

gObject* List::prev()
{
  win=win->prev();
  --_index;
  if(_index<0) _index=_length+_index+1;
  return win->_val;
}

gObject* List::first()
{
  win=header->next();
  _index=1;
  return win->_val;
}

gObject* List::last()
{
  win=header->prev();
  _index=_length;
  return win->_val;
}

int List::length()
{
  return _length;
}

int List::isFirst()
{
  return ((win==header->next()) && (_length > 0));
}

int List::isLast()
{
  return ((win==header->prev()) && (_length > 0));
}

int List::isHead()
{
  return (win==header);
}

//
// Stack
//

Stack::Stack()
{
}

Stack::~Stack()
{
}

void Stack::push(gObject *v)
{
  s.prepend(v);
}

gObject* Stack::pop()
{
  s.first();
  return s.remove();
}

int Stack::empty()
{
  return ((s.length()) == 0);
}

int Stack::size()
{
  return s.length();
}

gObject* Stack::top()
{
  return s.first();
}

gObject* Stack::nextToTop()
{
  s.first();
  return s.next();
}

gObject* Stack::bottom()
{
  return s.last();
}

//
// Ulility function
//

int* list2vector(List *list)
{
  int i,n,*out;
  n=list->length();
  out=ivector1_2(n);
  list->first();
  for(i=1;i<=n;i++){
    out[i]=((gInt*) list->val())->data;
    list->next();
  }
  return out;
}

#ifdef __cplusplus
}
#endif 
