/*
Lexical Analyser based on the bit-sequence algorithm designed by Sulzmann and Lu.

I verify that I am the sole author of the source code contained in this file, except where explicitly stated to the contrary.

This is a translation of the Scala code written in bitcode_lexer.sc to C++.
Hence, the credits I have given for various functions in bitcode_lexer.sc apply to this source code as well.

Source code in this file has been mostly adapted from the paper "POSIX Regular Expression Parsing with
Derivatives" by Sulzmann and Lu unless stated otherwise. All helper functions have been implemented by me unless stated otherwise.

Author: Arshdeep Singh Pareek.
Date: April 9, 2021.
*/

#include <iostream>
#include <string>
#include <typeinfo>
#include <vector>
#include <deque>
#include <set>
#include <utility>
#include <chrono>

using std::cout;
using std::string;
using std::vector;
using std::deque;
using std::set;
using std::pair;
using namespace std::chrono;
using std::endl;

// Class declarations for basic regular expressions.
// Structure taken from re3.sc coursework file provided in the 6CCS3CFL module.
class Rexp {
    public: string name;
            Rexp(string nameIn)
            : name(nameIn){

            }
            virtual bool operator== (Rexp & other){
                return name == other.name;
            }
            virtual bool equals (Rexp* other){
                return name == other->name;
            }
            virtual void operator= (Rexp & other){
                 name = other.name;
            }
            virtual ~Rexp(){
                cout << "Destruct Rexp\n"; 
            }

           
};

class ZERO : public Rexp {
    public:
            ZERO()
            :Rexp("ZERO"){

            }

            bool operator== (Rexp & other){
                if(other.name == "ZERO") {
                    return true;
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "ZERO") {
                    return true;
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                 Rexp::operator=(other);
            }
           
};
class ONE : public Rexp
{
    public:
            ONE()
            : Rexp("ONE"){

            }

            bool operator== (Rexp & other){
                if(other.name == "ONE") {
                    return true;
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "ONE") {
                    return true;
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                 Rexp::operator=(other);
            }
};
class CHAR : public Rexp
{
    public: char c;
            CHAR(char cIn)
            : Rexp("CHAR"), c(cIn){

            }
            CHAR(const CHAR & other)
            : Rexp("CHAR"), c(other.c){

            }
            char getC(){
                return c;
            }
            bool operator== (Rexp & other){
                if(other.name == "CHAR") {
                    CHAR* rexp = static_cast<CHAR*>(&other);
                    return (c == rexp->c);
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(name == "CHAR") {
                    CHAR* rexp = static_cast<CHAR*>(other);
                    return (c == rexp->c);
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                if(other.name == "CHAR") {
                    Rexp::operator=(other);
                    CHAR* rexp = static_cast<CHAR*>(&other);
                    c = rexp->c;
                }
            }
};
class ALT : public Rexp {
    public: Rexp* r1;
            Rexp* r2;
            ALT(Rexp* rIn1, Rexp* rIn2)
            : Rexp("ALT"), r1(rIn1), r2(rIn2){

            }
            bool operator== (Rexp & other){
                if(other.name == "ALT") {
                    ALT* rexp = static_cast<ALT*>(&other);
                    return (*r1 == *rexp->r1) && (*r2 == *rexp->r2);
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "ALT") {
                    ALT* rexp = static_cast<ALT*>(other);
                    return (r1->equals(rexp->r1)) && (r2->equals(rexp->r2));
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                if(other.name == "ALT") {
                    Rexp::operator=(other);
                    ALT* rexp = static_cast<ALT*>(&other);
                    Rexp rexp1 = *rexp->r1;
                    Rexp rexp2 = *rexp->r2;
                    r1 = &rexp1;
                    r2 = &rexp2;
                }
            }
};
class SEQ : public Rexp
{
    public: Rexp* r1;
            Rexp* r2;
            SEQ(Rexp* rIn1, Rexp* rIn2)
            : Rexp("SEQ"), r1(rIn1), r2(rIn2){
               
            }
            bool operator== (Rexp & other){
                if(other.name == "SEQ") {
                    SEQ* rexp = static_cast<SEQ*>(&other);
                    return (*r1 == *rexp->r1) && (*r2 == *rexp->r2);
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "SEQ") {
                    SEQ* rexp = static_cast<SEQ*>(other);
                    return (r1->equals(rexp->r1)) && (r2->equals(rexp->r2));
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                if(other.name == "SEQ") {
                    Rexp::operator=(other);
                    SEQ* rexp = static_cast<SEQ*>(&other);
                    Rexp rexp1 = *rexp->r1;
                    Rexp rexp2 = *rexp->r2;
                    r1 = &rexp1;
                    r2 = &rexp2;
                }
            }
};
class STAR : public Rexp
{
    public: Rexp* rs;
            STAR(Rexp* rsIn)
            : Rexp("STAR"), rs(rsIn){

            }
            bool operator== (Rexp & other){
                if(other.name == "STAR") {
                    STAR* rexp = static_cast<STAR*>(&other);
                    return (*rs == *rexp->rs);
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "STAR") {
                    STAR* rexp = static_cast<STAR*>(other);
                    return (rs->equals(rexp->rs));
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                if(other.name == "STAR") {
                    Rexp::operator=(other);
                    STAR* rexp = static_cast<STAR*>(&other);
                    Rexp r1 = *rexp->rs;
                    rs = &r1;
                }
            }
};
class NTIMES : public Rexp
{
    public: Rexp* rs;
            int n;
            NTIMES(Rexp* rsIn, int nIn)
            : Rexp("NTIMES"), rs(rsIn), n(nIn){

            }
            bool operator== (Rexp & other){
                if(other.name == "NTIMES") {
                    NTIMES* rexp = static_cast<NTIMES*>(&other);
                    return (*rs == *rexp->rs) && (n == rexp->n);
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "NTIMES") {
                    NTIMES* rexp = static_cast<NTIMES*>(other);
                    return (rs->equals(rexp->rs) && (n == rexp->n));
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                if(other.name == "NTIMES") {
                    Rexp::operator=(other);
                    NTIMES* rexp = static_cast<NTIMES*>(&other);
                    Rexp r1 = *rexp->rs;
                    rs = &r1;
                    n = rexp->n;
                }
            }
};

class RECD : public Rexp
{
    public: string x;
            Rexp* r;
            RECD(string xIn, Rexp* rIn)
            : Rexp("RECD"), x(xIn), r(rIn){

            }
            bool operator== (Rexp & other){
                if(other.name == "RECD") {
                    RECD* rexp = static_cast<RECD*>(&other);
                    string x2 = rexp->x;
                    Rexp* r2 = rexp->r;
                    return (x == x2) && (*r == *r2);
                }
                else{
                    return false;
                }
            }
            bool equals(Rexp* other){
                if(other->name == "RECD") {
                    RECD* rexp = static_cast<RECD*>(other);
                    string x2 = rexp->x;
                    Rexp* r2 = rexp->r;
                    return (x == x2) && (*r == *r2);
                }
                else{
                    return false;
                }
            }
            void operator= (Rexp & other){
                if(other.name == "RECD") {
                    Rexp::operator=(other);
                    RECD* rexp = static_cast<RECD*>(&other);
                    x = rexp->x;
                    Rexp r1 = *rexp->r;
                    r = &r1;
                }
            }
};

// Class declarations for basic regular expressions with annotations included.
class ARexp {
    public: string name;
            deque<bool> ann;
            ARexp(string nameIn)
            : name(nameIn), ann(deque<bool>(0)){

            }
            ARexp(deque<bool> annIn, string nameIn)
            : name(nameIn), ann(annIn){

            }

            // Virtual methods for checking equality.
            virtual bool operator== (ARexp & other){
                return name == other.name;
            }
            virtual bool equals (ARexp* other){
                return name == other->name;
            }

            void operator= (ARexp & other){
                 name = other.name;
                 ann = other.ann;
            }

            // Methods for adding booleans at the end of the bit-sequence.
            virtual void pushBack(bool in){
                ann.push_back(in);
            }
            virtual void pushBack(deque<bool> inList){
                int size = inList.size();
                for(int i = 0; i < size; i++){
                    ann.push_back(inList[i]);
                }
               
            }

            // Methods for adding booleans at the front of the bit-sequence.
            virtual void pushFront(bool in){
                ann.push_front(in);
            }
            virtual void pushFront(deque<bool> inList){
                int size = inList.size();
                for(int i = 0; i < size; i++){
                    ann.push_front(inList[(size-1) - i]);
                }
            }

            virtual int annSize(){
                return ann.size();
            }
               
};

class AZERO : public ARexp {
    public:
            AZERO()
            :ARexp("AZERO"){

            }

            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "AZERO") {
                    return true;
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "AZERO") {
                    return true;
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                 ARexp::operator=(other);
            }
            int annSize(){
                int size = ARexp::annSize();
                return size;
            }
           
};

class AONE : public ARexp {
    public:
            AONE()
            :ARexp("AONE"){

            }
            AONE(deque<bool> annIn)
            :ARexp(annIn, "AONE"){

            }

            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "AONE") {
                    return true;
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "AONE") {
                    return true;
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                 ARexp::operator=(other);
            }
            int annSize(){
                int size = ARexp::annSize();
                return size;
            }
           
};

class ACHAR : public ARexp
{
    public: char c;
            ACHAR(char cIn)
            : ARexp("ACHAR"), c(cIn){

            }
            ACHAR(deque<bool> annIn, char cIn)
            : ARexp(annIn, "ACHAR"), c(cIn){

            }
            char getC(){
                return c;
            }
            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "ACHAR") {
                    ACHAR* arexp = static_cast<ACHAR*>(&other);
                    return (c == arexp->c);
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "ACHAR") {
                    ACHAR* rexp = static_cast<ACHAR*>(other);
                    return (c == rexp->c);
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                if(name == "ACHAR") {
                    ARexp::operator=(other);
                    ACHAR* arexp = static_cast<ACHAR*>(&other);
                    c = arexp->c;
                }
            }
            int annSize(){
                int size = ARexp::annSize();
                return size;
            }
};

class AALT : public ARexp {
    public: deque<ARexp*> rs;
            AALT(deque<ARexp*> rsIn)
            : ARexp("AALT"), rs(rsIn){

            }
            AALT(deque<bool> annIn, deque<ARexp*> rsIn)
            : ARexp(annIn, "AALT"), rs(rsIn){

            }

            // Checks for equality between two deques.
            bool dequeEquals(deque<ARexp*> rs1, deque<ARexp*>rs2){
                if(rs1.size() == rs2.size()){
                    for(int i = 0; i < rs1.size(); i++){
                        if(!(*rs1[i] == *rs2[i])){
                            return false;
                        }
                    }
                    return true;
                }
                else{
                    return false;
                }
            }

            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "AALT") {
                    AALT* rexp = static_cast<AALT*>(&other);
                    return (dequeEquals(rs, rexp->rs));
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "AALT") {
                    AALT* rexp = static_cast<AALT*>(other);
                    deque<ARexp*> rs1 = rexp->rs;
                    if(rs1.size() != rs.size()){
                        return false;
                    }
                    for(int i = 0; i < rs1.size(); ++i){
                        if(!rs[i]->equals(rs1[i])){
                            return false;
                        }
                    }
                    return true;
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                if(name == "AALT") {
                    ARexp::operator=(other);
                    AALT* arexp = static_cast<AALT*>(&other);
                    deque<ARexp*> rs1 = arexp->rs;
                    rs = rs1;
                }
                
            }
            int annSize(){
                int size = ARexp::annSize();
                for(int i = 0; i < rs.size(); ++i){
                    size += rs[i]->annSize();
                }
                return size;
            }
};

class ASEQ : public ARexp
{
    public: ARexp* r1;
            ARexp* r2;
            ASEQ(ARexp* rIn1, ARexp* rIn2)
            : ARexp("ASEQ"), r1(rIn1), r2(rIn2){
               
            }
            ASEQ(deque<bool> annIn, ARexp* rIn1, ARexp* rIn2)
            : ARexp(annIn, "ASEQ"), r1(rIn1), r2(rIn2){
               
            }

            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "ASEQ") {
                    ASEQ* rexp = static_cast<ASEQ*>(&other);
                    return (*r1 == *rexp->r1) && (*r2 == *rexp->r2);
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "ASEQ") {
                    ASEQ* rexp = static_cast<ASEQ*>(other);
                    return (r1->equals(rexp->r1)) && (r2->equals(rexp->r2));
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                if(name == "ASEQ") {
                    ARexp::operator=(other);
                    ASEQ* arexp = static_cast<ASEQ*>(&other);
                    ARexp rexp1 = *arexp->r1;
                    ARexp rexp2 = *arexp->r2;
                    r1 = &rexp1;
                    r2 = &rexp2;
                }
            }
            int annSize(){
                int size = ARexp::annSize();
                size += r1->annSize();
                size += r2->annSize();
                return size;
            }
};

class ASTAR : public ARexp
{
    public: ARexp* rs;
            ASTAR(ARexp* rsIn)
            : ARexp("ASTAR"), rs(rsIn){

            }
            ASTAR(deque<bool> annIn, ARexp* rsIn)
            : ARexp(annIn, "ASTAR"), rs(rsIn){

            }

            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "ASTAR") {
                    ASTAR* rexp = static_cast<ASTAR*>(&other);
                    return (*rs == *rexp->rs);
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "ASTAR") {
                    ASTAR* rexp = static_cast<ASTAR*>(other);
                    return (rs->equals(rexp->rs));
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                if(name == "ASTAR") {
                    ARexp::operator=(other);
                    ASTAR* arexp = static_cast<ASTAR*>(&other);
                    ARexp rexp = *arexp->rs;
                    rs = &rexp;
                }
            }
            int annSize(){
                int size = ARexp::annSize();
                size += rs->annSize();
                return size;
            }
};
class ANTIMES : public ARexp
{
    public: ARexp* rs;
            int n;
            ANTIMES(ARexp* rsIn, int nIn)
            : ARexp("ANTIMES"), rs(rsIn), n(nIn){

            }
            ANTIMES(deque<bool> annIn, ARexp* rsIn, int nIn)
            : ARexp(annIn, "ANTIMES"), rs(rsIn), n(nIn){

            }

            // Methods for checking equality between this regular expression and another.
            bool operator== (ARexp & other){
                if(other.name == "ANTIMES") {
                    ANTIMES* rexp = static_cast<ANTIMES*>(&other);
                    return (*rs == *rexp->rs) && (n == rexp->n);
                }
                else{
                    return false;
                }
            }
            bool equals(ARexp* other){
                if(other->name == "ANTIMES") {
                    ANTIMES* rexp = static_cast<ANTIMES*>(other);
                    return (rs->equals(rexp->rs) && (n == rexp->n));
                }
                else{
                    return false;
                }
            }

            void operator= (ARexp & other){
                if(name == "ANTIMES") {
                    ARexp::operator=(other);
                    ANTIMES* arexp = static_cast<ANTIMES*>(&other);
                    ARexp* rexp = arexp->rs;
                    rs = rexp;
                    n = arexp->n;
                }
            }
            int annSize(){
                int size = ARexp::annSize();
                size += rs->annSize();
                return size;
            }
};

// Class declarations for different values.
// Adapted from lexer.sc coursework file provided in the 6CCS3CFL module except for noMatch value.
// noMatch value helps test (a*)*b and (a+a+)+b regular expressions.
class Val {
    public: string name;
            Val(string nameIn)
            : name(nameIn){

            }
            virtual ~Val(){

            }
};
class noMatch : public Val
{
    public:
            noMatch()
            : Val("noMatch"){

            }
};
class Empty : public Val
{
    public:
            Empty()
            : Val("Empty"){

            }
};
class Chr : public Val
{
    public:
            char c;
            Chr(char cIn)
            : Val("Chr"), c(cIn){

            }
};
class Left : public Val
{
    public:
            Val* leftVal;
            Left(Val* leftIn)
            : Val("Left"), leftVal(leftIn){

            }
};
class Right : public Val
{
    public:
            Val* rightVal;
            Right(Val* rightIn)
            : Val("Right"), rightVal(rightIn){

            }
};
class Sequ : public Val
{
    public:
            Val* val1;
            Val* val2;
            Sequ(Val* val1In, Val* val2In)
            : Val("Sequ"), val1(val1In), val2(val2In){

            }
};
class Stars : public Val
{
    public:
            deque<Val*> vals;
            Stars(deque<Val*> valsIn)
            : Val("Stars"), vals(valsIn){

            }
};
class Ntimes : public Val
{
    public:
            deque<Val*> vals;
            Ntimes(deque<Val*> valsIn)
            : Val("Ntimes"), vals(valsIn){

            }
};
class Rec : public Val
{
    public:
            string x;
            Val* v;
            Rec(string xIn, Val* valIn)
            : Val("Rec"), x(xIn), v(valIn){

            }
};

// Concatenates any given bit sequence to the existing annotation of an 
// annotated regular expression.
ARexp* fuse(deque<bool> bs, ARexp* r){
    string name = r->name;
    if(name == "AZERO"){
        return r;
    }
    else{
        r->pushFront(bs);
        return r;
    }
}

// Concatenates any given bit to the existing annotation of an 
// annotated regular expression.
ARexp* fuse(bool bs, ARexp* r){
    string name = r->name;
    if(name == "AZERO"){
        return r;
    }
    else{
        r->pushFront(bs);
        return r;
    }
}

// Helper function converts a list of regular expressions into a nested ALT regular expression.
Rexp* listAlt(deque<Rexp*> list){
    int size = list.size();
    if(size == 0){
        return new ZERO();
    }
    else if(size == 1){
        return list[0];
    }
    else{
        Rexp* front = list.front();
        list.pop_front();
        return new ALT(front, listAlt(list));
    }
}

// Converts annotated regular expression to unannotated regular expressions.
// Adapted from code provided by Dr. Urban.
Rexp* deannotate(ARexp* ar){
    string name = ar->name;
    if(name == "AZERO"){
        return new ZERO();
    }
    else if(name == "AONE") {
        return new ONE();
    }
    else if(name == "ACHAR") {
        ACHAR* rexp = static_cast<ACHAR*>(ar);
        char c = rexp->c;
        return new CHAR(c);
    }
    else if(name == "AALT"){
        AALT* rexp = static_cast<AALT*>(ar);
        deque<ARexp*> rs1 = rexp->rs;
        deque<Rexp*> deannotatedRs = deque<Rexp*>{};
        for(int i = 0; i < rs1.size(); ++i){
            deannotatedRs.push_back(deannotate(rs1[i]));
        }
        return listAlt(deannotatedRs);
    }
    else if(name == "ASEQ"){
        ASEQ* rexp = static_cast<ASEQ*>(ar);
        ARexp* rexp1 = rexp->r1;
        ARexp* rexp2 = rexp->r2;
        return new SEQ(deannotate(rexp1), deannotate(rexp2));
    }
    else if(name == "ASTAR"){
        ASTAR* rexp = static_cast<ASTAR*>(ar);
        ARexp* rs1 = rexp->rs;
        return new STAR(deannotate(rs1));
    }
    else if(name == "ANTIMES"){
        ANTIMES* rexp = static_cast<ANTIMES*>(ar);
        ARexp* rs1 = rexp->rs;
        int n1 = rexp->n;
        return new NTIMES(deannotate(rs1), n1);
    }
    else{
        cout << "error in deannotate" << endl;
        return new ZERO();
    }
}

// Helper function checks whether an element is present in a set.
bool setContains(set<ARexp*> & rSet, ARexp* r){
    for(auto elem : rSet){
        if(elem->equals(r)){
            return true;
        }
    }
    return false;
}

// Auxiliary function filters out duplicates of the same regular expression from a list, keeping 
// only the first instance.
deque<ARexp*> distinct(deque<ARexp*> rs){
    set<ARexp*> rexpSet = set<ARexp*>{};
    deque<ARexp*> uniqueRs = deque<ARexp*>{};
    for(int i = 0; i < rs.size(); ++i){
        ARexp* currRaexp = rs[i];
        if(!setContains(rexpSet, currRaexp)){
            uniqueRs.push_back(currRaexp);
            rexpSet.insert(currRaexp);
        }
    }
    return uniqueRs; 
}

// Inserts an empty bit sequence to a regular expression, converting it to 
// an annotated regular expression.
ARexp* internalize(Rexp* r){
    string name = r->name;
    if(name == "ZERO") {
        return new AZERO();
    }
    else if(name == "ONE") {
        return new AONE();
    }
    else if(name == "CHAR") {
        CHAR* rexp = static_cast<CHAR*>(r);
        char c = rexp->c;
        return new ACHAR(c);
    }
    else if(name == "ALT") {
        ALT* rexp = static_cast<ALT*>(r);
        ARexp* intR1 = fuse(false, internalize(rexp->r1));
        ARexp* intR2 = fuse(true, internalize(rexp->r2));
        //fuse(false, internalize(rexp->r1));
        //fuse(true, internalize(rexp->r2));
        return new AALT(deque<ARexp*>{intR1, intR2});
    }
    else if(name == "SEQ") {
        SEQ* rexp = static_cast<SEQ*>(r);
        ARexp* intR1 = internalize(rexp->r1);
        ARexp* intR2 = internalize(rexp->r2);
        return new ASEQ(intR1, intR2);
    }
    else if(name == "STAR") {
        STAR* rexp = static_cast<STAR*>(r);
        ARexp* intR = internalize(rexp->rs);
        return new ASTAR(intR);
    }
    else if(name == "NTIMES") {
        NTIMES* rexp = static_cast<NTIMES*>(r);
        ARexp* intR = internalize(rexp->rs);
        int n1 = rexp->n;
        return new ANTIMES(intR, n1);
    }
    else if(name == "RECD") {
        RECD* rRecd = static_cast<RECD*>(r);
        ARexp* intR = internalize(rRecd->r);
        return intR;
    }
    return new AZERO();
}

// Helper function to append a list rs2 at the end of list rs1.
void push_Back(deque<bool> & rs1, deque<bool> rs2){
                int size = rs2.size();
                for(int i = 0; i < size; i++){
                    rs1.push_back(rs2[i]);
                }
            }
// Helper function to append a list rs2 at the end of list rs1.
template <typename T>
deque<T> push_Back(deque<T> rs1, deque<T> rs2){
                int size = rs2.size();
                for(int i = 0; i < size; i++){
                    rs1.push_back(rs2[i]);
                }
                return rs1;
            }

// Helper function to make deep copies of regular expression pointers.
// It copies the regular expression attached to the pointer and returns a pointer to the copy.
Rexp* deepCopyRegex(Rexp* reg){

    string name = reg->name;

    if(name == "ZERO") {
        return new ZERO();
    }
    else if(name == "ONE"){
        Rexp* outOne = new ONE();
        return outOne;
    }
    else if (name == "CHAR"){
        CHAR* rexp = static_cast<CHAR*>(reg);
        char cReg = rexp->c;
        Rexp* outChar = new CHAR(cReg);
        return outChar;
    }
    else if(name == "ALT"){
        ALT* rexp = static_cast<ALT*>(reg);
        Rexp* copyR1 = deepCopyRegex(rexp->r1);
        Rexp* copyR2 = deepCopyRegex(rexp->r2);
        Rexp* outAlt = new ALT(copyR1, copyR2);
        return outAlt;
    }
    else if(name == "SEQ"){
        SEQ* rexp = static_cast<SEQ*>(reg);
        Rexp* copyR1 = deepCopyRegex(rexp->r1);
        Rexp* copyR2 = deepCopyRegex(rexp->r2);
        Rexp* outSeq = new SEQ(copyR1, copyR2);
        return outSeq;
    }
    else if(name == "STAR"){
        STAR* rexp = static_cast<STAR*>(reg);
        Rexp* copyRs = deepCopyRegex(rexp->rs);
        Rexp* outStar = new STAR(copyRs);
        return outStar;
    }
    else if(name == "NTIMES"){
        NTIMES* rexp = static_cast<NTIMES*>(reg);
        Rexp* copyRs = deepCopyRegex(rexp->rs);
        int n1 = rexp->n;
        Rexp* outNTimes = new NTIMES(copyRs, n1);
        return outNTimes;
    }

    else if(name == "RECD"){
        RECD* rexp = static_cast<RECD*>(reg);
        Rexp* copyR = deepCopyRegex(rexp->r);
        string copyX = rexp->x;
        Rexp* outRecd = new RECD(copyX, copyR);
        return outRecd;
    }

    cout << name << "fault in copy\n";
    return new ZERO();
}

// Helper function to make deep copies of annotated regular expression pointers.
// It copies the annotated regular expression attached to the input pointer and 
// returns a pointer to the copy.
ARexp* deepCopyRegex(ARexp* areg){

    string name = areg->name;

    if(name == "AZERO") {
        return new AZERO();
    }
    else if(name == "AONE"){
        deque<bool> annReg = areg->ann;
        ARexp* outOne = new AONE(annReg);
        return outOne;
    }
    else if (name == "ACHAR"){
        deque<bool> annReg = areg->ann;
        ACHAR* rexp = static_cast<ACHAR*>(areg);
        char cReg = rexp->c;
        ARexp* outChar = new ACHAR(annReg, cReg);
        return outChar;
    }
    else if(name == "AALT"){
        deque<bool> annReg = areg->ann;
        AALT* rexp = static_cast<AALT*>(areg);
        deque<ARexp*> & rs = rexp->rs;
        deque<ARexp*> copyRs = deque<ARexp*>{};
        for(int i = 0; i < rs.size(); ++i){
            copyRs.push_back(deepCopyRegex(rs[i]));
        }
        ARexp* outAlt = new AALT(annReg, copyRs);
        return outAlt;
    }
    else if(name == "ASEQ"){
        deque<bool> annReg = areg->ann;
        ASEQ* rexp = static_cast<ASEQ*>(areg);
        ARexp* copyR1 = deepCopyRegex(rexp->r1);
        ARexp* copyR2 = deepCopyRegex(rexp->r2);
        ARexp* outSeq = new ASEQ(annReg, copyR1, copyR2);
        return outSeq;
    }
    else if(name == "ASTAR"){
        deque<bool> annReg = areg->ann;
        ASTAR* rexp = static_cast<ASTAR*>(areg);
        ARexp* copyRs = deepCopyRegex(rexp->rs);
        ARexp* outStar = new ASTAR(annReg, copyRs);
        return outStar;
    }
    else if(name == "ANTIMES"){
        deque<bool> annReg = areg->ann;
        ANTIMES* rexp = static_cast<ANTIMES*>(areg);
        ARexp* copyRs = deepCopyRegex(rexp->rs);
        int n1 = rexp->n;
        ARexp* outANTimes = new ANTIMES(annReg, copyRs, n1);
        return outANTimes;
    }

    cout << name << "fault in copy\n";
    return new AZERO();
}

// Helper function to make a deep copy of a list of annotated regular expressions.
// Takes as input a list of annotated regular expressions and returns another list with
// copies of the input annotated regular expressions.
deque<ARexp*> deepCopyRegexList(deque<ARexp*> & aregs){
    deque<ARexp*> outList = deque<ARexp*>{};
    deque<ARexp*> & rs1 = aregs;
    for(int i = 0; i < rs1.size(); ++i){
        outList.push_back(deepCopyRegex(rs1[i]));
    }
    return outList;
}

// Determines if a regular expression can match the empty string. 
bool nullableBC(ARexp* r){
    string name = r->name;
    if(name == "AZERO") {return false;}
    else if(name == "AONE") {return true;}
    else if(name == "ACHAR") {return false;}
    else if(name == "AALT") {
        AALT* rexp = static_cast<AALT*>(r);
        if(rexp->rs.size() == 1){
            return nullableBC(rexp->rs[0]);
        }
        else{
            deque<ARexp*> rs1 = rexp->rs;
            ARexp* first = rs1.front();
            rs1.pop_front();
            return nullableBC(first) || nullableBC(new AALT(rs1));
        }
    }
    else if(name == "ASEQ") {
        ASEQ* rexp = static_cast<ASEQ*>(r);
        return nullableBC(rexp->r1) && nullableBC(rexp->r2);
        }
    else if(name == "ASTAR") {return true;}
    else if(name == "ANTIMES") {
        ANTIMES* rexp = static_cast<ANTIMES*>(r);
            if(rexp->n == 0){
                return true;
            }
            else{
                return nullableBC(rexp->rs);
            }
        }
    else {return false;}
}

// Removes ZERO regular expressions from alternative regular expressions.
deque<ARexp*> flatten(deque<ARexp*> & rs){
    if(rs.size() == 0){
        return deque<ARexp*>{};
    }
    else if(rs.front()->name == "AZERO"){
        rs.pop_front();
        return flatten(rs);
    }
    else if(rs.front()->name == "AALT"){
        AALT* rexp = static_cast<AALT*>(rs.front());
        deque<ARexp*> rs1 = rexp->rs;
        deque<bool> ann = rexp->ann;
        for(int i = 0; i < rs1.size(); i++){
            ARexp* rexp1 = rs1[i];

            rs1[i] = fuse(ann, rexp1);
        }
        rs.pop_front();
        auto rs2 = flatten(rs);
        for(int j = 0; j < rs2.size(); j++){
            rs1.push_back(rs2[j]);
        }
        return rs1;
    }
    else{
        ARexp* r1 = rs.front();
        rs.pop_front();
        auto rsFlat = flatten(rs);
        rsFlat.push_front(r1);
        return rsFlat;
    }


}

// Simplifies regular expressions in the intermediate steps of the Brzozowski matching algorithm.
// Adapted from simplification rules provided in Chengsong Tan's paper.
ARexp* simpBC(ARexp* r){
    string name = r->name;
    if(name == "ASEQ") {
        ASEQ* rexp = static_cast<ASEQ*>(r);
        ARexp* simpR1 = simpBC(rexp->r1);
        ARexp* simpR2 = simpBC(rexp->r2);
        if(simpR1->name == "AZERO"){return simpR1;}
        else if(simpR2->name == "AZERO"){return simpR2;}
        else if(simpR1->name == "AONE"){
            deque<bool> ann1 = rexp->ann;
            deque<bool> ann2 = simpR1->ann;
            push_Back(ann1, ann2);
            ARexp* simpR2Copy = deepCopyRegex(simpR2);
            fuse(ann1, simpR2Copy);
            return simpR2Copy;}
        else if(simpR1->name == "AALT"){
            AALT* sr1 = static_cast<AALT*>(simpR1);
            deque<ARexp*> & rs1 = sr1->rs;
            for(int i = 0; i < rs1.size(); ++i){
                ARexp* currRexp = rs1[i];
                rs1[i] = new ASEQ(currRexp, simpR2);
            }
            deque<bool> anns = rexp->ann;
            deque<bool> simpAnn = sr1->ann;
            push_Back(anns, simpAnn);
            AALT* outRexp = new AALT(anns, rs1);
            return outRexp;
        }
        else {
            ARexp* outRexp = new ASEQ(simpR1, simpR2);
            outRexp->pushBack(rexp->ann);
            return outRexp;
            }
    }
    else if(name == "AALT"){
        AALT* rexp = static_cast<AALT*>(r);
        deque<ARexp*> rs1 = rexp->rs;
        int size = rs1.size();
        
        for(int i = 0; i < size; i++){
            ARexp* currRexp = rs1[i];
            rs1[i] = simpBC(currRexp);
        }
        deque<ARexp*> flatRs = distinct(flatten(rs1));
        if(flatRs.size() == 0){
            return new AZERO();
        }
        else if(flatRs.size() == 1){
            ARexp* flatRsCopy = deepCopyRegex(flatRs.front());
            deque<bool> ann1 = rexp->ann;
            return fuse(ann1, flatRsCopy);
        }
        else{
            deque<ARexp*> flatRsCopy = deepCopyRegexList(flatRs);
            deque<bool> ann1 = rexp->ann;
            ARexp* outRexp = new AALT(ann1, flatRsCopy);
            return outRexp;
        }
    }
    else{
        return r;
    }
}

// Returns a bit sequence of how the given regular expression matches the empty string.
deque<bool> mkepsBC(ARexp* r){
    string name = r->name;
    if(name == "AONE") {return r->ann;}
    else if(name == "AALT") {
        AALT* rexp = static_cast<AALT*>(r);
        deque<ARexp*> & rs = rexp->rs;
        ARexp* r1 = rs.front();
        rs.pop_front();
        deque<bool> rAnn = r->ann;
        if(nullableBC(r1)){
            deque<bool> mkepsR1 = mkepsBC(r1);
            push_Back(rAnn, mkepsR1);
            deque<bool> outAnn = rAnn;
            return outAnn;
        }
        else{
            ARexp* remainRexp = new AALT(rAnn, rs);
            deque<bool> mkepsRexp = mkepsBC(remainRexp);
            return mkepsRexp;
        }
    }
    else if(name == "ASEQ") {
        ASEQ* rexp = static_cast<ASEQ*>(r);
        ARexp* r1 = rexp->r1;
        ARexp* r2 = rexp->r2;
        deque<bool> mkepsR1 = mkepsBC(r1);
        deque<bool> mkepsR2 = mkepsBC(r2);
        deque<bool> rAnn = r->ann;
        deque<bool> outAnn = rAnn;
        push_Back(outAnn, mkepsR1);
        push_Back(outAnn, mkepsR2);
        return outAnn;
    }
    else if(name == "ASTAR"){
        deque<bool> outAnn = r->ann;
        outAnn.push_back(true);
        return outAnn;
    }
    else if(name == "ANTIMES"){
        deque<bool> outAnn = r->ann;
        ANTIMES* rexp = static_cast<ANTIMES*>(r);
        int n1 = rexp->n;
        deque<bool> mkepsAnn = mkepsBC(rexp->rs);
        for(int i = 0; i < n1; ++i){
            push_Back(outAnn, mkepsAnn);
        }
        return outAnn;
    }
    else{
        return deque<bool>{};
    }
}

// Returns the derivative of the input annotated regular expression with respect to the input character.
ARexp* derBC(char c, ARexp* r){
    string name = r->name;
    if(name == "AZERO") {return r;}
    else if(name == "AONE") {
        return new AZERO();
    }
    else if(name == "ACHAR") {
        ACHAR* rexp = static_cast<ACHAR*>(r);
        if(c == rexp->c){
            deque<bool> ann1 = rexp->ann;
            ARexp* ONE = new AONE(ann1);
            return ONE;
        }
        else{
            return new AZERO();
        }
    }
    else if(name == "AALT"){
        AALT* rexp = static_cast<AALT*>(r);
        deque<ARexp*> & rs = rexp->rs; 
        deque<ARexp*> rsCopy = deque<ARexp*>{};
        for(int i = 0; i < rs.size(); ++i){
            ARexp* currRexp = deepCopyRegex(rs[i]);
            rsCopy.push_back(derBC(c, currRexp));
        }
        deque<bool> ann1 = rexp->ann;
        ARexp* outAALT = new AALT(ann1, rsCopy);
        return outAALT;
    }
    else if(name == "ASEQ"){
        ASEQ* rexp = static_cast<ASEQ*>(r);
        ARexp* rexp1 = rexp->r1;
        ARexp* rexp2 = rexp->r2;
        if(nullableBC(rexp1)){
            deque<bool> mkepsR1 = mkepsBC(rexp1);
            ARexp* derR2 = derBC(c, rexp2);
            deque<ARexp*> rs = deque<ARexp*>{new ASEQ(derBC(c, rexp1), rexp2), fuse(mkepsR1, derR2)};
            ARexp* outAALT = new AALT(rs);
            outAALT->pushBack(rexp->ann);
            return outAALT;
        }
        else{
            deque<bool> ann1 = rexp->ann;
            ASEQ* outASEQ = new ASEQ(derBC(c, rexp1), rexp2);
            outASEQ->pushBack(ann1);
            return outASEQ;
        }
    }
    else if(name == "ASTAR"){
        ASTAR* rexp = static_cast<ASTAR*>(r);
        deque<bool> ann = rexp->ann;
        ARexp* rs = deepCopyRegex(rexp->rs);
        ASEQ* outASEQ = new ASEQ(ann, fuse(false, derBC(c, rs)), new ASTAR(rs));
        return outASEQ;
    }
    else if(name == "ANTIMES"){
        ANTIMES* rexp = static_cast<ANTIMES*>(r);
        int n1 = rexp->n;
        if(n1 == 0){
            return new AZERO();
        }
        deque<bool> ann1 = rexp->ann;
        ARexp* rs = deepCopyRegex(rexp->rs);
        ASEQ* outASEQ = new ASEQ(ann1, derBC(c, rs), new ANTIMES(rs, n1-1));
        return outASEQ;
    }
    else{
        return new AZERO();
    }
}


ARexp* ders(string s , ARexp* r){
    if(s.length() == 0){return r;}
    else if(s.length() == 1){
        ARexp* r1 = simpBC(r);
        return derBC(s[0], r1);
        }
    else{
        ARexp* r1 = derBC(s[0], r);
        ARexp* simpR1 = simpBC(r1);
        return ders(s.substr(1, s.size()-1), simpR1);
        }
}

// Returns the size or the number of nodes in a regular expression.
// Adapted from re3.sc coursework file from 6CCS3CFL module.
int regexSize(Rexp* r){
    string name = r->name;
    if(name == "ZERO") {return 1;}
    else if(name == "ONE") { return 1;}
    else if(name == "CHAR") { return 1;}
    else if(name == "ALT") { 
        ALT* rexp = static_cast<ALT*>(r);
        return 1 + regexSize(rexp->r1) + regexSize(rexp->r2);
    }
    else if(name == "SEQ") { 
        SEQ* rexp = static_cast<SEQ*>(r);
        Rexp* r1 = rexp->r1;
        Rexp* r2 = rexp->r2;
        return 1 + regexSize(r1) + regexSize(r2);
    }
    else if(name == "STAR") { 
        STAR* rexp = static_cast<STAR*>(r);
        Rexp* rs = rexp->rs;
        return 1 + regexSize(rs);
    }
    else if(name == "NTIMES") { 
        NTIMES* rexp = static_cast<NTIMES*>(r);
        Rexp* rs = rexp->rs;
        return 1 + regexSize(rs);
    }
    else{
        cout << "error in determining regular expression size." << endl;
        return 0;
    }
}

// Returns the size or the number of nodes in an annotated regular expression.
// Adapted from re3.sc coursework file from 6CCS3CFL module.
int regexSizeBC(ARexp* r){
    string name = r->name;
    if(name == "AZERO") {return 1;}
    else if(name == "AONE") { return 1;}
    else if(name == "ACHAR") { return 1;}
    else if(name == "AALT") { 
        AALT* rexp = static_cast<AALT*>(r);
        deque<ARexp*> & rs = rexp->rs; 
        int size = 1;
        for(int i = 0; i < rs.size(); ++i){
            size += regexSizeBC(rs[i]);
        }
        return size;
    }
    else if(name == "ASEQ") { 
        ASEQ* rexp = static_cast<ASEQ*>(r);
        ARexp* r1 = rexp->r1;
        ARexp* r2 = rexp->r2;
        return 1 + regexSizeBC(r1) + regexSizeBC(r2);
    }
    else if(name == "ASTAR") { 
        ASTAR* rexp = static_cast<ASTAR*>(r);
        ARexp* rs = rexp->rs;
        return 1 + regexSizeBC(rs);
    }
    else if(name == "ANTIMES") { 
        ANTIMES* rexp = static_cast<ANTIMES*>(r);
        ARexp* rs = rexp->rs;
        return 1 + regexSizeBC(rs);
    }
    else{
        cout << "error in determining regular expression size." << endl;
        return 0;
    }
}

//Helper function to reverse a deque of strings.
deque<string> reverse(deque<string> list){
    deque<string> outList = deque<string>{};
    int size = list.size();
    for(int i = size-1; i >= 0; --i){
        string currStr = list[i];
        outList.push_back(currStr);
    }
    return outList;
}

//Helper functions to convert a deque of strings to a single string.
template <typename T>
string listToString(deque<T> s){
    string out = "";
    int sLen = s.size();
    for(int i = 0; i < sLen; i++){
        out += s[i];
    }
    return out;
}

string listToString(deque<bool> s){
    string out = "";
    int sLen = s.size();
    for(int i = 0; i < sLen; i++){
        if(s[i]){
            out += "1";
        }
        else{
            out += "0";
        }
        
    }
    return out;
}

// Decodes and tokenizes an input value based on input bit-sequence.
// Tail-recursive version of decode using an accumulator string.
// Adapted from code provided by Dr. Urban.
deque<string> sdecode_aux(deque<Rexp*> rs, deque<bool> bs, deque<string> acc){
    if(rs.size() == 0 ){
        return acc;
    }
    
    Rexp* rf = rs.front();
    string name = rf->name;
    rs.pop_front();
    
    if(name == "ONE"){
        return sdecode_aux(rs, bs, acc);
    }
    else if(name == "CHAR"){
        
        CHAR* rChar = static_cast<CHAR*>(rf);
        char c = rChar->c;
        string & head = acc.front();
        head += c;
        return sdecode_aux(rs, bs, acc);
    }
    else if(name == "ALT"){
        if(bs.size() == 0){
            return acc;
        }
        ALT* rAlt = static_cast<ALT*>(rf);
        bool front = bs.front();
        bs.pop_front();
        if(front == false){
            Rexp* r1 = rAlt->r1;
            rs.push_front(r1);
            return sdecode_aux(rs, bs, acc);
        }
        else{
            Rexp* r2 = rAlt->r2;
            rs.push_front(r2);
            return sdecode_aux(rs, bs, acc);
        }
    }
    else if(name == "SEQ"){
        SEQ* rSeq = static_cast<SEQ*>(rf);
        Rexp* r1 = rSeq->r1;
        Rexp* r2 = rSeq->r2;
        rs.push_front(r2);
        rs.push_front(r1);
        return sdecode_aux(rs, bs, acc);
    }
    else if(name == "STAR"){
        if(bs.size() == 0){
            return acc;
        }
        bool front = bs.front();
        bs.pop_front();
        if(front == false){
            STAR* rStar = static_cast<STAR*>(rf);
            Rexp* rs1 = rStar->rs;
            rs.push_front(rStar);
            rs.push_front(rs1);
            return sdecode_aux(rs, bs, acc);
        }
        else{
            return sdecode_aux(rs, bs, acc);
        }
        
    }
    else if(name == "RECD"){
        RECD* rRecd = static_cast<RECD*>(rf);
        Rexp* r1 = rRecd->r;
        string x1 = rRecd->x;
        //rs.pop_front();
        rs.push_front(r1);
        x1 += ":";
        acc.push_front(x1);
        return sdecode_aux(rs, bs, acc);
    }
    else{
        return deque<string>{};
    }
}

deque<string> sdecode(Rexp* r, deque<bool> bs){
    deque<string> reversedList = sdecode_aux(deque<Rexp*>{r}, bs, deque<string>{""});
    deque<string> tokList = reverse(reversedList);
    return tokList;
}

// Converts input bit-sequences and input regular expression to values.
pair<Val*, deque<bool>> decode(Rexp* r, deque<bool> bs){
    string name = r->name;
    if(name == "ONE"){
        return pair<Val*, deque<bool>>(new Empty(), bs);
    }
    else if(name == "CHAR"){
        CHAR* rexp = static_cast<CHAR*>(r);
        char c1 = rexp->c;
        return pair<Val*, deque<bool>>(new Chr(c1), bs);
    }
    else if(name == "ALT"){
        ALT* rexp = static_cast<ALT*>(r);
        bool frontBit = bs.front();
        bs.pop_front();
        if(frontBit == false){
            Rexp* rexp1 = deepCopyRegex(rexp->r1);
            pair<Val*, deque<bool>> p1 = decode(rexp1, bs);
            return pair<Val*, deque<bool>>(new Left(p1.first), p1.second);
        }
        else{
            Rexp* rexp2 = deepCopyRegex(rexp->r2);
            pair<Val*, deque<bool>> p2 = decode(rexp2, bs);
            return pair<Val*, deque<bool>>(new Right(p2.first), p2.second);
        }
    }
    else if(name == "SEQ"){
        SEQ* rexp = static_cast<SEQ*>(r);
        Rexp* rexp1 = deepCopyRegex(rexp->r1);
        Rexp* rexp2 = deepCopyRegex(rexp->r2);
        pair<Val*, deque<bool>> p1 = decode(rexp1, bs);
        pair<Val*, deque<bool>> p2 = decode(rexp2, p1.second);
        return pair<Val*, deque<bool>>(new Sequ(p1.first, p2.first), p2.second);
    }
    else if(name == "STAR"){
        STAR* rexp = static_cast<STAR*>(r);
        if(bs.size() == 0){
            return pair<Val*, deque<bool>>(new Stars(deque<Val*>{}), bs);
        }
        bool frontBit = bs.front();
        bs.pop_front();
        if(frontBit == false){
            Rexp* rexps = deepCopyRegex(rexp->rs);
            pair<Val*, deque<bool>> p1 = decode(rexps, bs);
            pair<Val*, deque<bool>> p2 = decode(rexp, p1.second);
            Val* vs = p2.first;
            Stars* val1 = static_cast<Stars*>(vs);
            deque<Val*> s = val1->vals;
            s.push_front(p1.first);
            return pair<Val*, deque<bool>>(new Stars(s), p2.second);
        }
        else{
            return pair<Val*, deque<bool>>(new Stars(deque<Val*>{}), bs);
        }
    }
    else if(name == "NTIMES"){
        NTIMES* rexp = static_cast<NTIMES*>(r);
        int n1 = rexp->n;
        if(n1 == 0){
            return pair<Val*, deque<bool>>(new Ntimes(deque<Val*>{}), bs);
        }
        
        Rexp* rexps1 = deepCopyRegex(rexp->rs);
        Rexp* rexps2 = deepCopyRegex(rexp->rs);
        
        pair<Val*, deque<bool>> p1 = decode(rexp->rs, bs);
        
        pair<Val*, deque<bool>> p2 = decode(new NTIMES(rexps2, n1 - 1), p1.second);
        Val* vs = p2.first;
        Ntimes* val1 = static_cast<Ntimes*>(vs);
        deque<Val*> s = val1->vals;
        s.push_front(p1.first);
        
        return pair<Val*, deque<bool>>(new Ntimes(s), p2.second);
    }
    else if(name == "RECD"){
        RECD* rexp = static_cast<RECD*>(r);
        Rexp* rs = rexp->r;
        pair<Val*, deque<bool>> p = decode(rs, bs);
        return pair<Val*, deque<bool>>(new Rec(rexp->x, p.first), p.second);
    }
    else{
        return pair<Val*, deque<bool>>(new Empty(), deque<bool>{});
    }
}

// Returns the underlying matched string under the given value.
// Adapted from my submission for coursework 2 in the 6CCS3CFL module.
string flattenVal(Val* v) {
    string name = v->name;
    if(name == "Empty"){
        return "";
    }
    else if(name == "Chr"){
        Chr* v1 = static_cast<Chr*>(v);
        string c = string(1, v1->c);
        return c;
    }
    else if(name == "Left"){
        Left* v1 = static_cast<Left*>(v);
        Val* vs = v1->leftVal;
        return flattenVal(vs);
    }
    else if(name == "Right"){
        Right* v1 = static_cast<Right*>(v);
        Val* vs = v1->rightVal;
        return flattenVal(vs);
    }
    else if(name == "Sequ"){
        Sequ* v1 = static_cast<Sequ*>(v);
        Val* vs1 = v1->val1;
        Val* vs2 = v1->val2;
        string fl1 = flattenVal(vs1);
        string fl2 = flattenVal(vs2);
        return fl1 + fl2;
    }
    else if(name == "Stars"){
        Stars* v1 = static_cast<Stars*>(v);
        deque<Val*> vs = v1->vals;
        string fl = "";
        int size = vs.size();
        for(int i = 0; i < size; ++i){
            fl += flattenVal(vs[i]);
        }
        return fl;
    }
    else if(name == "Ntimes"){
        Ntimes* v1 = static_cast<Ntimes*>(v);
        deque<Val*> vs = v1->vals;
        string fl = "";
        int size = vs.size();
        for(int i = 0; i < size; ++i){
            fl += flattenVal(vs[i]);
        }
        return fl;
    }
    else if(name == "Rec"){
        Rec* v1 = static_cast<Rec*>(v);
        Val* vs1 = v1->v;
        return flattenVal(vs1);
    }
    else{
        cout << "error in flattenVal function" << endl;
        return "";
    }
}

// Returns a list of tokens extracted from RECD regular expressions.
// Adapted from my submission for coursework 2 in the 6CCS3CFL module.
deque<pair<string, string>> env(Val* v){
    string name = v->name;
    if(name == "Empty"){
        return deque<pair<string, string>>{};
    }
    else if(name == "Chr"){
        return deque<pair<string, string>>{};
    }
    else if(name == "Left"){
        Left* v1 = static_cast<Left*>(v);
        Val* vs = v1->leftVal;
        return env(vs);
    }
    else if(name == "Right"){
        Right* v1 = static_cast<Right*>(v);
        Val* vs = v1->rightVal;
        return env(vs);
    }
    else if(name == "Sequ"){
        Sequ* v1 = static_cast<Sequ*>(v);
        Val* vs1 = v1->val1;
        Val* vs2 = v1->val2;
        return push_Back(env(vs1), env(vs2));
    }
    else if(name == "Stars"){
        Stars* v1 = static_cast<Stars*>(v);
        deque<Val*> vs = v1->vals;
        auto tmp = deque<deque<pair<string, string>>>{};
        auto out = deque<pair<string, string>>{};
        int size = vs.size();
        for(int i = 0; i < size; ++i){
            Val* curr = vs[i];
            tmp.push_back(env(curr));
        }
        for(int i = 0; i < tmp.size(); ++i){
            deque<pair<string, string>> & curr = tmp[i];
            for(int j = 0; j < curr.size(); ++j){
                out.push_back(curr[j]);
            }
        }
        return out;
    }
    else if(name == "Ntimes"){
        Ntimes* v1 = static_cast<Ntimes*>(v);
        deque<Val*> vs = v1->vals;
        auto tmp = deque<deque<pair<string, string>>>{};
        auto out = deque<pair<string, string>>{};
        int size = vs.size();
        for(int i = 0; i < size; ++i){
            Val* curr = vs[i];
            tmp.push_back(env(curr));
        }
        for(int i = 0; i < tmp.size(); ++i){
            deque<pair<string, string>> & curr = tmp[i];
            for(int j = 0; j < curr.size(); ++j){
                out.push_back(curr[j]);
            }
        }
        return out;
    }
    else if(name == "Rec"){
        Rec* v1 = static_cast<Rec*>(v);
        string x1 = v1->x;
        Val* vs1 = v1->v;
        deque<pair<string, string>> out = env(vs1);
        out.push_front(pair<string, string>(x1, flattenVal(vs1)));
        return out;
    }
    else{
        cout << "error in env function" << endl;
        return deque<pair<string, string>>{pair<string, string>("", "")};
    }
}

// Helper function to return value names as a string from values.
string valToString(Val* v){
    string name = v->name;
    if(name == "Empty"){
        return "Empty";
    }
    else if(name == "Chr"){
        Chr* v1 = static_cast<Chr*>(v);
        string c = string(1, v1->c);
        return "Chr(\'" + c + "\')";
    }
    else if(name == "Left"){
        Left* v1 = static_cast<Left*>(v);
        Val* vs = v1->leftVal;
        return "Left(" + valToString(vs) + ")";
    }
    else if(name == "Right"){
        Right* v1 = static_cast<Right*>(v);
        Val* vs = v1->rightVal;
        return "Right(" + valToString(vs) + ")";
    }
    else if(name == "Sequ"){
        Sequ* v1 = static_cast<Sequ*>(v);
        Val* vs1 = v1->val1;
        Val* vs2 = v1->val2;
        return "Sequ(" + valToString(vs1) + ", " + valToString(vs2) + ")";
    }
    else if(name == "Stars"){
        Stars* v1 = static_cast<Stars*>(v);
        deque<Val*> vs = v1->vals;
        string out = "Stars(";
        int size = vs.size();
        for(int i = 0; i < size; ++i){
            if(i == size-1){
                out += valToString(vs[i]);
            }
            else{
                out += valToString(vs[i]) + ", ";
            }
        }
        out += ")";
        return out;
    }
    else if(name == "Ntimes"){
        Ntimes* v1 = static_cast<Ntimes*>(v);
        deque<Val*> vs = v1->vals;
        string out = "Ntimes(";
        int size = vs.size();
        for(int i = 0; i < size; ++i){
            if(i == size-1){
                out += valToString(vs[i]);
            }
            else{
                out += valToString(vs[i]) + ", ";
            }
        }
        out += ")";
        return out;
    }
    else{
        return "error in valToString";
    }

}

// Recursively applies the derivative to a regular expression with respect to a given string
// and simplifies intermediate regular expressions.
ARexp* simpDersBC(deque<char> s, ARexp* r){
    if(s.size() == 0){
        return r;
    }
    else{
        char c = s.front();
        s.pop_front();
        return simpDersBC(s, simpBC(derBC(c, r)));
    }
}

// Helper function to convert a string to a list of characters.
deque<char> stringToList(string s){
    deque<char> charList = deque<char>(0);
    int sLen = s.length();
    for(int i = 0; i < sLen; i++){
        char currChar = s[i];
        charList.push_back(currChar);
    }
    return charList;
}

// Returns a value associated with matching the input string s with respect 
// to the input regular expression r. It needs further processing for tokenisation.
Val* blexer_simp(Rexp* r, deque<char> s){
    ARexp* a = simpDersBC(s, internalize(r));
    //Used to measure the size of the final regular expression.
    //cout << "Size: " << regexSize(deannotate(a)) << endl;
    if(nullableBC(a)){
        return decode(r, mkepsBC(a)).first;
    }
    else{
        cout << "lexing error...\n";
        return new noMatch();
    }
}

// Tokenises the input string with respect to the input regular expression
// using the tail-recursive decode function, sdecode. 
deque<string> blexer2_simp(Rexp* r, string s){
    ARexp* a = simpDersBC(stringToList(s), internalize(r));
    //Used to measure the size of the final regular expression.
    //cout << "Size: " << regexSize(deannotate(a)) << endl;
    if(nullableBC(a)){
        return sdecode(r, mkepsBC(a));
    }
    else{
        cout << "No match found.\n";
        return deque<string>{};
    }
}

// Helper function to convert a string into a nested SEQ regular expression
// representing the ordered concatenation of all the characters in the string.
Rexp* stringToSEQ(string s){
    if(s.length() == 1){
        return new CHAR(s[0]);
    }
    else{
        return new SEQ(new CHAR(s[0]), stringToSEQ(s.substr(1, s.size()-1)));
    }
}

// Helper function to convert a string into a nested ALT regular expression
// representing the RANGE regular expression.
Rexp* RANGE(string s){
    if(s.length() == 1){
        return new CHAR(s[0]);
    }
    else{
        return new ALT(new CHAR(s[0]), RANGE(s.substr(1, s.size()-1)));
    }
}

// Helper function to return the tail elements of a deque.
deque<Rexp*> listTail(deque<Rexp*> ls){
    deque<Rexp*> outList = deque<Rexp*>{};
    for(int i = 1; i < ls.size(); ++i){
        outList.push_back(ls[i]);
    }
    return outList;
}

// Helper function to convert a deque of regular expressions into a 
// nested ALT regular expression.
Rexp* listToALT(deque<Rexp*> ls){
    if(ls.size() == 1){
        return ls[0];
    }
    else{
        return new ALT(ls[0], listToALT(listTail(ls)));
    }
}


// *** THE FOLLOWING CODE IS FOR TESTING AND EXPERIMENT PURPOSES.***


// Performs tests on the simp function to ensure correct output for each if-elseif branch.
void simpFunctionTest(){
    bool test1 = (simpBC(new AONE(deque<bool>{}))->equals(new AONE(deque<bool>{})));
    cout << test1 << endl;
    bool test2 = (simpBC(new ACHAR(deque<bool>{}, 'z'))->equals(new ACHAR(deque<bool>{}, 'z')));
    cout << test2 << endl;
    bool test3 = (simpBC(new ASTAR(deque<bool>{}, new AONE(deque<bool>{false})))->equals(new ASTAR(deque<bool>{}, new AONE(deque<bool>{false}))));
    cout << test3 << endl;
    bool test4 = (simpBC(new ANTIMES(deque<bool>{true}, new AONE(deque<bool>{false}), 8))->equals(new ANTIMES(deque<bool>{true}, new AONE(deque<bool>{false}), 8)));
    cout << test4 << endl;
    bool test5 = (simpBC(new ASEQ(deque<bool>{true}, new ACHAR(deque<bool>{}, 'b'), new AZERO()))->equals(new AZERO()));
    cout << test5 << endl;
    bool test6 = (simpBC(new ASEQ(deque<bool>{true}, new AZERO(), new ACHAR(deque<bool>{}, 'b')))->equals(new AZERO()));
    cout << test6 << endl;
    bool test7 = (simpBC(new ASEQ(deque<bool>{true}, new AONE(deque<bool>{false, true}), new ACHAR(deque<bool>{true}, 'b')))->equals(new ACHAR(deque<bool>{true, false, true, true}, 'b')));
    cout << test7 << endl;
    bool test8 = (simpBC(new ASEQ(deque<bool>{true}, new AALT(deque<bool>{false}, deque<ARexp*>{new AONE(deque<bool>{}), new ACHAR(deque<bool>{}, 'c')}), new AONE(deque<bool>{false})))->equals(new AALT(deque<bool>{true, false}, deque<ARexp*>{new ASEQ(deque<bool>{}, new AONE(deque<bool>{}), new AONE(deque<bool>{false})), new ASEQ(deque<bool>{}, new ACHAR(deque<bool>{}, 'c'), new AONE(deque<bool>{false}))})));
    cout << test8 << endl;
    bool test9 = (simpBC(new AALT(deque<bool>{true}, deque<ARexp*>{}))->equals(new AZERO()));
    cout << test9 << endl;
    bool test10 = (simpBC(new AALT(deque<bool>{true}, deque<ARexp*>{new AONE(deque<bool>{false})}))->equals(new AONE(deque<bool>{true, false})));
    cout << test10 << endl;
    bool test11 = (simpBC(new AALT(deque<bool>{true}, deque<ARexp*>{new AONE(deque<bool>{false}), new AONE(deque<bool>{true})}))->equals(new AONE(deque<bool>{true, false})));
    cout << test11 << endl;
    bool test12 = (simpBC(new AALT(deque<bool>{true}, deque<ARexp*>{new AONE(deque<bool>{false}), new AONE(deque<bool>{true}), new ACHAR(deque<bool>{}, 'a')}))->equals(new AALT(deque<bool>{true}, deque<ARexp*>{new AONE(deque<bool>{false}), new ACHAR(deque<bool>{}, 'a')})));
    cout << test12 << endl;
}

// Performs tests on the mkepsBC function to ensure correct output for each if-elseif branch.
void mkepsFunctionTest(){
    bool test1 = (mkepsBC(new AONE(deque<bool>{false, true})) == deque<bool>{false, true});
    cout << test1 << endl;
    bool test2 = (mkepsBC(new AALT(deque<bool>{false, true}, deque<ARexp*>{new AONE(deque<bool>{true})})) == deque<bool>{false, true, true});
    cout << test2 << endl;
    bool test3 = (mkepsBC(new AALT(deque<bool>{false, true}, deque<ARexp*>{new AONE(deque<bool>{true}), new ASTAR(deque<bool>{true}, new ACHAR(deque<bool>{}, 'a'))})) == deque<bool>{false, true, true});
    cout << test3 << endl;
    bool test4 = (mkepsBC(new AALT(deque<bool>{false, true}, deque<ARexp*>{new ACHAR(deque<bool>{}, 'a'), new AONE(deque<bool>{false})})) == deque<bool>{false, true, false});
    cout << test4 << endl;
    bool test5 = (mkepsBC(new ASEQ(deque<bool>{false, true}, new AONE(deque<bool>{true}), new AONE(deque<bool>{}))) == deque<bool>{false, true, true});
    cout << test5 << endl;
    bool test6 = (mkepsBC(new ASTAR(deque<bool>{false}, new ACHAR(deque<bool>{}, 'a'))) == deque<bool>{false, true});
    cout << test6 << endl;
    bool test7 = (mkepsBC(new ANTIMES(deque<bool>{false}, new AONE(deque<bool>{false, true}), 0)) == deque<bool>{false});
    cout << test7 << endl;
    bool test8 = (mkepsBC(new ANTIMES(deque<bool>{false}, new AONE(deque<bool>{false, true}), 1)) == deque<bool>{false, false, true});
    cout << test8 << endl;
    bool test9 = (mkepsBC(new ANTIMES(deque<bool>{false}, new AONE(deque<bool>{false, true}), 5)) == deque<bool>{false, false, true, false, true, false, true, false, true, false, true});
    cout << test9 << endl;
}

// Performs tests on the derBC function to ensure correct output for each if-elseif branch.
void derFunctionTest(){
    ARexp* AZER = new AZERO();
    bool test1 = (derBC('a', new AZERO())->equals(AZER));
    cout << test1 << endl;
    bool test2 = (derBC('a', new AONE(deque<bool>{}))->equals(AZER));
    cout << test2 << endl;
    bool test3 = (derBC('a', new ACHAR(deque<bool>{}, 'a'))->equals(new AONE(deque<bool>{})));
    cout << test3 << endl;
    bool test4 = (derBC('a', new ACHAR(deque<bool>{}, 'c'))->equals(AZER));
    cout << test4 << endl;
    bool test5 = (derBC('a', new AALT(deque<bool>{false, true}, deque<ARexp*>{AZER, new AONE(deque<bool>{}), new ACHAR(deque<bool>{},'b')}))->equals(new AALT(deque<bool>{false, true}, deque<ARexp*>{AZER, AZER, AZER})));
    cout << test5 << endl;
    bool test6 = (derBC('a', new AALT(deque<bool>{false, true}, deque<ARexp*>{AZER, new AONE(deque<bool>{}), new ACHAR(deque<bool>{},'a')}))->equals(new AALT(deque<bool>{false, true}, deque<ARexp*>{AZER, AZER, new AONE(deque<bool>{})})));
    cout << test6 << endl;
    bool test7 = (derBC('a', new ASEQ(deque<bool>{}, new ACHAR(deque<bool>{},'a'), new AONE(deque<bool>{})))->equals(new ASEQ(deque<bool>{}, new AONE(deque<bool>{}), new AONE(deque<bool>{}))));
    cout << test7 << endl;
    bool test8 = (derBC('a', new ASEQ(deque<bool>{false, true}, new AONE(deque<bool>{false}), new ACHAR(deque<bool>{},'a')))->equals(new AALT(deque<bool>{false, true}, deque<ARexp*>{new ASEQ(deque<bool>{}, AZER, new ACHAR(deque<bool>{}, 'a')), new AONE(deque<bool>{false})})));
    cout << test8 << endl;
    bool test9 = (derBC('a', new ANTIMES(deque<bool>{}, new ACHAR(deque<bool>{},'a'), 0))->equals(AZER));
    cout << test9 << endl;
    bool test10 = (derBC('a', new ANTIMES(deque<bool>{}, new ACHAR(deque<bool>{},'a'), 1))->equals(new ASEQ(deque<bool>{}, new AONE(deque<bool>{}), new ANTIMES(deque<bool>{}, new ACHAR(deque<bool>{}, 'a'), 0))));
    cout << test10 << endl;
    bool test11 = (derBC('a', new ANTIMES(deque<bool>{}, new ACHAR(deque<bool>{},'a'), 2))->equals(new ASEQ(deque<bool>{}, new AONE(deque<bool>{}), new ANTIMES(deque<bool>{}, new ACHAR(deque<bool>{}, 'a'), 1))));
    cout << test11 << endl;
    bool test12 = (derBC('a', new ASTAR(deque<bool>{false}, new ACHAR(deque<bool>{}, 'a')))->equals(new ASEQ(deque<bool>{false}, new AONE(deque<bool>{false}), new ASTAR(deque<bool>{}, new ACHAR(deque<bool>{}, 'a')))));
    cout << test12 << endl;
}

// Represents the PLUS(r) regular expression as its equivalent SEQ(r, STAR(r))
Rexp* PLUS(Rexp* r){
    return new SEQ(r, new STAR(r));
}

// Helper function to create a RECD regular expression with a token xIn and 
// regular expression rIn.
Rexp* mkRECD(string xIn, Rexp* rIn){
    return new RECD(xIn, rIn);
}

// Represents the OPTIONAL(r) regular expression as its equivalent ALT(ONE, r)
Rexp* OPTIONAL(Rexp* rIn){
    return new ALT(new ONE(), rIn);
}

int main() {
    //Function calls to test important functions.
    //derFunctionTest();
    //mkepsFunctionTest();
    //simpFunctionTest();
    
    // Defining the regular expressions for the WHILE language.
    Rexp* SYM = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_._><;=,:\\");
    Rexp* DIGIT = RANGE("0123456789");
    Rexp* ID = new SEQ(SYM, new STAR(new ALT(SYM, DIGIT)));
    Rexp* NUM = new ALT(new CHAR('0'), new SEQ(RANGE("123456789"), new STAR(DIGIT)));
    Rexp* KEYWORD = listToALT(deque<Rexp*>{stringToSEQ("skip"), stringToSEQ("while"), stringToSEQ("do"), stringToSEQ("if"), stringToSEQ("then"), stringToSEQ("else"), stringToSEQ("read"), stringToSEQ("write")});
    Rexp* SEMI = new CHAR(';');
    Rexp* OP = listToALT(deque<Rexp*>{stringToSEQ("+"), stringToSEQ("-"), stringToSEQ("*"), stringToSEQ("/"), stringToSEQ("%"), stringToSEQ(":="), stringToSEQ("!="), stringToSEQ("="), stringToSEQ("<"), stringToSEQ(">")});
    Rexp* WHITESPACE = PLUS(listToALT(deque<Rexp*>{new CHAR(' '), new CHAR('\n'), new CHAR('\t')}));
    Rexp* PARANTHESES = RANGE("({)}");
    Rexp* STR = new SEQ(new CHAR('\"'), new SEQ(new ALT(new STAR(SYM), new ALT(WHITESPACE, DIGIT)), new CHAR('\"')));

    Rexp* WHILE_REGS = new STAR(listToALT(deque<Rexp*>{mkRECD("k", KEYWORD), mkRECD("i", ID), mkRECD("o", OP), mkRECD("n", NUM), mkRECD("s", SEMI), mkRECD("str", STR), mkRECD("p", PARANTHESES), mkRECD("w", WHITESPACE)}));
    
    
    // Sample WHILE programs for experiments and testing.
    // WHILE program for calculating fibonacci numbers.
    // Taken from the coursework file lexer.sc in 6CCS3CFL module.
    string progFib = R"(write "Fib";
read n;
minus1 := 0;
minus2 := 1;
while n > 0 do {
  temp := minus2;
  minus2 := minus1 + minus2;
  minus1 := temp;
  n := n - 1
};
write "Result";
write minus2)";

    // WHILE program for calculating factorials. This is used to conduct experiments.
    // Taken from my answer to HW08 in 6CCS3CFL module.
    string progFac = R"(read (n);
factorial := 1;
i := 1;
while i <= n do {
 factorial := factorial * i;
 i := i + 1
};
result := factorial;
write (result))";

    // Used to collect data for each type of experiment.
    for(int i = 0; i <= 150; i += 10){
        string prog = progFac;
        // Used for testing repetitions of program source code
        /* 
        for(int j = 1; j < i; ++j){
            prog += progFac;
        } */
        unsigned long duration_total = 0;
        int iterations = 5;
        for(int i = 0; i < iterations; ++i){
            auto startTime = high_resolution_clock::now();
            //((a*)*b))
            listToString(blexer2_simp(mkRECD("(a*)*b)", new SEQ(new STAR(new STAR(new CHAR('a'))), new CHAR('b'))), string(i, 'a')));

            //((1+a){n}(a){n})
            //listToString(blexer2_simp(mkRECD("(1+a){n}(a){n}", new SEQ(new NTIMES(new ALT(new ONE(),(new CHAR('a'))), i), new NTIMES(new CHAR('a'), i))), string(i, 'a')));

            //(a+aa)*
            //listToString(blexer2_simp(mkRECD("(a+aa)*", new STAR(new ALT(new CHAR('a'), new SEQ(new CHAR('a'), new CHAR('a'))))), string(i, 'a')));

            //Tokenising WHILE program.
            //listToString(blexer2_simp(WHILE_REGS, prog));

            //(((a+)(a+))+)b
            //listToString(blexer2_simp(mkRECD("triplePlus", new SEQ(PLUS(new SEQ(PLUS(new CHAR('a')), PLUS(new CHAR('a')))), new CHAR('b'))), string(i, 'a')));

            auto endTime = high_resolution_clock::now();
            unsigned long duration = duration_cast<std::chrono::nanoseconds>(endTime - startTime).count(); 
            duration_total += duration;
            
        }
        float average_duration = duration_total/iterations;
        cout << average_duration << " nanoseconds" << endl;
    }

    // Tokenizes the factorial program and prints it to the console.
    // cout << listToString(blexer2_simp(WHILE_REGS, progFac)) << endl;

    return 0;
}