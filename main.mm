//
//  main.cpp
//  GTAAssembler
//
//  Created by Greg Slomin on 9/6/14.
//  Copyright (c) 2014 Greg Slomin. All rights reserved.
//
#import <Foundation/Foundation.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <assert.h>
#include <string.h>
#include <algorithm>
using namespace std;
using std::cout;
using std::istringstream;
typedef uint8_t Byte;
bool isComment(string str);
#define YSC
NSArray* tTables();
NSDictionary* nativeTable();

NSString *getHashKey(NSString* orig, NSArray *tables) {
    for(NSDictionary *table : tables) {
        NSString *temp = table[orig];
        if(temp != nil)
            orig = temp;
    }
    return orig;
}



uint64_t rotr(uint64_t x, uint64_t r) {
    r = r%64;
    return (x << (64 - r)) | (x >> r);

}
NSNumber* joaat(char *key, size_t len)
{
    uint32_t hash, i;
    for(hash = i = 0; i < len; ++i)
    {
        hash += key[i];
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return @(hash);
}
NSNumber* getHashFromJoaat(NSNumber *key, NSArray *tables) {
    NSString *temp = nativeTable()[key];
    NSMutableArray *objs =[[nativeTable() allKeysForObject:temp] mutableCopy];
    [objs removeObject:key];
    return objs.firstObject;
}



NSArray* tTables()
{
    static NSArray *tables = nil;
    if(tables == nil) {
        NSString *path = @"/Users/gregslomin/saved_files 2/GTAAssembler/native_translation.json";//[[NSBundle mainBundle] pathForResource:@"native_translation" ofType:@"json"];
        
        
        NSError *error;
        NSDictionary *translationDic = [NSJSONSerialization JSONObjectWithData:[NSData dataWithContentsOfFile:path] options:kNilOptions error:&error];
        
        tables = translationDic[@"translation_tables"];
    }
    return tables;
}

NSDictionary* nativeTable()
{
    static NSDictionary *nativesDic = nil;
    if(nativesDic != nil)
        return nativesDic;
    //    if(nativesDic == nil)
    //    {
    //        NSString *path = [[NSBundle mainBundle] pathForResource:@"natives" ofType:@"txt"];
    //        NSError *error;
    //        NSData *data = [NSData dataWithContentsOfFile:path options:0 error:&error];
    //        nativesDic = [XSCDisassembler dictionaryFromNatives:data];
    //    }
    
    NSString *path = @"/Users/gregslomin/saved_files 2/GTAAssembler/natives.json";//[[NSBundle mainBundle] pathForResource:@"natives" ofType:@"json"];
    NSError *error;
    NSData *data = [NSData dataWithContentsOfFile:path];
    
    
    NSDictionary *dic = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
    
    NSArray *tables = tTables();
    
    
    NSArray *keys = [dic allKeys];
    NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
    [formatter setNumberStyle:NSNumberFormatterDecimalStyle];
    
    NSMutableDictionary *finalNatives = [[NSMutableDictionary alloc] init];
    for(NSString *str : keys) {
        NSDictionary *o = dic[str];
        NSArray *moreKeys = [o allKeys];
        for(NSString *moreStr : moreKeys) {
            NSDictionary *native = o[moreStr];
            NSString *hash = native[@"jhash"];
            
            NSString * key = getHashKey([moreStr substringFromIndex:2], tables);
            
            NSScanner* pScanner = [NSScanner scannerWithString: key];
            
            uint64_t iValue;
            //[pScanner scanHexInt: &iValue];
            [pScanner scanHexLongLong:&iValue];
            
            NSNumber *hashNumber  = @(iValue);//@(strtoll(hash.cString, NULL, 0));
            
            if([native[@"name"] length] > 0 && hashNumber != nil) {
                [finalNatives setObject:[((NSString*)native[@"name"]) lowercaseString] forKey:hashNumber];
                //  [finalNatives setObject:[((NSString*)native[@"name"]) lowercaseString] forKey:@(strtoll(hash.cString, NULL, 0))];
            }
        }
        
    }
    nativesDic = finalNatives;
    return nativesDic;
}



NSDictionary* jTables()
{
    static NSDictionary *nativesDic = nil;
    if(nativesDic != nil)
        return nativesDic;
    //    if(nativesDic == nil)
    //    {
    //        NSString *path = [[NSBundle mainBundle] pathForResource:@"natives" ofType:@"txt"];
    //        NSError *error;
    //        NSData *data = [NSData dataWithContentsOfFile:path options:0 error:&error];
    //        nativesDic = [XSCDisassembler dictionaryFromNatives:data];
    //    }
    
    NSString *path = @"/Users/gregslomin/saved_files 2/GTAAssembler/natives.json";//[[NSBundle mainBundle] pathForResource:@"natives" ofType:@"json"];
    NSError *error;
    NSData *data = [NSData dataWithContentsOfFile:path];
    
    
    NSDictionary *dic = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
    
    NSArray *tables = tTables();
    
    
    NSArray *keys = [dic allKeys];
    NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
    [formatter setNumberStyle:NSNumberFormatterDecimalStyle];
    
    NSMutableDictionary *finalNatives = [[NSMutableDictionary alloc] init];
    for(NSString *str : keys) {
        NSDictionary *o = dic[str];
        NSArray *moreKeys = [o allKeys];
        for(NSString *moreStr : moreKeys) {
            NSDictionary *native = o[moreStr];
            NSString *hash = native[@"jhash"];
            
            NSString * key = getHashKey([moreStr substringFromIndex:2], tables);
            
            NSScanner* pScanner = [NSScanner scannerWithString: key];
            
            uint64_t iValue;
            //[pScanner scanHexInt: &iValue];
            [pScanner scanHexLongLong:&iValue];
            
            NSNumber *hashNumber  = @(iValue);//@(strtoll(hash.cString, NULL, 0));
            
            if([native[@"name"] length] > 0 && hashNumber != nil) {
                //[finalNatives setObject:[((NSString*)native[@"name"]) lowercaseString] forKey:hashNumber];
                  [finalNatives setObject:hashNumber forKey:@(strtoll(hash.cString, NULL, 0))];
            }
        }
        
    }
    nativesDic = finalNatives;
    return nativesDic;
}

struct SCHeader {
    
    uint32_t VTable ; //just makes them properties, nothing more.
    uint32_t subheader ;
    uint32_t code_blocks_offset ;
    uint32_t globals_version ;
    uint32_t code_length ;
    uint32_t parameter_count ;
    uint32_t localsCount ;
    uint32_t globals_count ;
    uint32_t natives_count ;
    uint32_t localsOffset ;
    uint32_t globals_offset ;
    uint32_t natives_table ;
    uint32_t null4 ;
    uint32_t null5 ;
    uint32_t crc ;
    uint32_t string_block_count ;
    uint32_t script_name_offset ;
    uint32_t strings_offset ;
    uint32_t strings_size ;
    uint32_t null6 ;
    SCHeader() {
        memset(this, 0, sizeof(SCHeader));
        VTable = 874988800;
        globals_version = 4260798006;
        
    }
    
};

struct YSCHeader {
    
    uint64_t vtable; // 0x0
    uint64_t subheader; // 0x08
    union {
        uint64_t code_blocks_offset; // 0x10
        uint64_t *code;
    };
    uint32_t globals_version; // 0x18

    uint32_t code_size; // 0x1C
    uint32_t param_count; // 0x20
    uint32_t statics_size; // 0x24
    uint32_t globals_size; // 0x28
    uint32_t natives_size; // 0x2C
    union { // // 0x30
        uint64_t statics_offset;
        uint64_t *statics;
    };
    union { // 0x38
    uint64_t globals_offset;
        uint64_t *globals;
    };
    
    union { // 0x40
    uint64_t natives_offset;
        uint64_t *natives;
    };
    
    uint64_t null1; // 0x48
    uint64_t null2; // 0x50
    uint32_t script_name_hash; // 0x54
    uint32_t string_blocks_count; // 0x58
    uint64_t script_name_offset; // 0x60
    union {
    uint64_t string_table_offset; // 0x68
        uint8_t *strings;
    };
    uint64_t string_table_size; // 0x70
    
    YSCHeader() {
        memset(this, 0, sizeof(SCHeader));
        vtable = 5369085472L;
        globals_version = 992938094;
        param_count = 0;
        code_size = string_table_size = statics_size= globals_size = natives_size = 0;
        string_blocks_count = 1;
        null1 = null2 = NULL;
        
    }
    
    uint64_t getFileSize() {
        int blockCount = 0;
        if(code_size != 0)
            blockCount += (code_size / 0x4000) + ((code_size % 0x4000) != 0);
        
        if(globals_size != 0)
            blockCount += (globals_size / 0x4000) + ((globals_size % 0x4000) != 0);
        
        
        if(string_table_size != 0)
            blockCount += (string_table_size / 0x4000) + ((string_table_size % 0x4000) != 0);
        
        if(statics_size > 0)
            blockCount += (statics_size / 0x4000) + ((statics_size % 0x4000) != 0);
        
        if(natives_size > 0)
            blockCount ++;
    
        return (0x80 + (blockCount * 0x4000) + blockCount*8);
        
        
    }
    
    uint64_t *writeBlocks(uint64_t *currPos, uint8_t *buffer, uint64_t *data, uint64_t size) {
        int num_blocks = (size / 0x4000) + ((size % 0x4000) != 0);
        uint64_t *blocks = new uint64_t[num_blocks];
        
        int pos = 0;
        for(int i=0; i<num_blocks; i++) {
            blocks[i] = (*currPos) | 0x50000000;
            if((size - pos) >= 0x4000)
                memcpy(buffer+*currPos, ((uint8_t*)data)+(pos), 0x4000);
            else
                memcpy(buffer+*currPos, ((uint8_t*)data)+(pos), (size - pos));
            pos += 0x4000;
            *currPos += 0x4000;
        }
        return blocks;
    }
    
    int numBlocks(int size) {
        return (size / 0x4000) + ((size % 0x4000) > 0);
    }
    uint8_t *createFile() {
        uint64_t size = getFileSize();
        uint8_t *buffer = new uint8_t[size];
        memset(buffer, 0, size);
        
        uint64_t currPos = 0x80;
        uint64_t *code_blocks = NULL;
        uint64_t *global_blocks = NULL;
        uint8_t *string_blocks = NULL;
        if(code_size > 0)
            code_blocks = writeBlocks(&currPos, buffer, code, code_size);
        
        if(globals_size > 0)
            global_blocks = writeBlocks(&currPos, buffer, globals, globals_size);
        
        if(string_table_size > 0)
            string_blocks = (uint8_t*)writeBlocks(&currPos, buffer, (uint64_t*)strings, string_table_size);
        
        
        
        if(natives_size > 0) {
            memcpy(buffer+currPos, natives, natives_size*8);
            natives_offset = currPos | 0x50000000;
            currPos += natives_size*8;
            currPos += (currPos%0x4000);
        }
        
        if(statics_size > 0) {
            memcpy(buffer+currPos, statics, statics_size);
            statics_offset = currPos | 0x50000000;
            currPos += statics_size;
            currPos += (currPos%0x4000);
        } else {
            statics_offset = 0;
        }
        
        if(code_size > 0) {
            memcpy(buffer+currPos, code_blocks, numBlocks(code_size)*8);
            code_blocks_offset = currPos | 0x50000000;
            delete[] code_blocks;
            currPos+=numBlocks(code_size)*8;
        } else {
            code_blocks_offset = 0;
        }

        if(globals_size > 0) {
            memcpy(buffer+currPos, global_blocks, numBlocks(globals_size)*8);
            globals_offset = currPos | 0x50000000;
            currPos+=numBlocks(globals_size)*8;
            delete[] global_blocks;
        } else {
            globals_offset = 0;
        }
        
        if(string_table_size > 0) {
            memcpy(buffer+currPos, string_blocks, numBlocks(string_table_size)*8);
            string_table_offset = currPos | 0x50000000;
            currPos+=numBlocks(string_table_size)*8;
            delete[] string_blocks;
        } else {
            string_table_offset = 0;
        }
        
        memcpy(buffer+currPos, "charanimtest", strlen("charanimtest"));
        
        script_name_offset = currPos | 0x50000000;
        

        char tempBuf[32] = {"charanimtest"};
        script_name_hash = joaat(tempBuf, strlen("charanimtest")).unsignedLongValue;
        memcpy(buffer, this, sizeof(YSCHeader));
        
    
        return buffer;
    }
    
};
struct Line
{
public:
    vector<string> tokens;
    string orig;
};
inline Byte u8imm(string str)
{
    uint64_t val = stoll(str.c_str());
    assert((val&0xFF) == val);
    return val & 0xFF;
}

inline uint32_t u16imm(string str)
{
    uint64_t val = stoll(str.c_str());
    assert((val&0xFFFF) == val);
    return val & 0xFFFF;
}

inline uint32_t u24imm(string str)
{
    uint64_t val = stoll(str.c_str());
    assert((val&0xFFFFFF) == val);
    return val & 0xFFFFFF;
}

inline uint32_t u32imm(string str)
{
    uint64_t val = stoll(str.c_str());
    assert((val&0xFFFFFFFF) == val);
    return val & 0xFFFFFFFF;
}

inline char s8imm(string str)
{
    int64_t val = stoll(str.c_str());
    assert((val&0xFF) == val);
    return val & 0xFF;
}

inline int32_t s24imm(string str)
{
    int64_t val = stoll(str.c_str());
    assert((val&0xFFFFFF) == val);
    return val & 0xFFFFFF;
}

inline int32_t s32imm(string str)
{
    int64_t val = stoll(str.c_str());
    assert((val&0xFFFFFFFF) == val);
    return val & 0xFFFFFFFF;
}
std::string trim(const std::string& str,
                 const std::string& whitespace = " \t")
{
    const auto strBegin = str.find_first_not_of(whitespace);
    if (strBegin == std::string::npos)
        return ""; // no content
    
    const auto strEnd = str.find_last_not_of(whitespace);
    const auto strRange = strEnd - strBegin + 1;
    
    return str.substr(strBegin, strRange);
}

std::string reduce(const std::string& str,
                   const std::string& fill = " ",
                   const std::string& whitespace = " \t")
{
    // trim first
    auto result = trim(str, whitespace);
    
    // replace sub ranges
    auto beginSpace = result.find_first_of(whitespace);
    while (beginSpace != std::string::npos)
    {
        const auto endSpace = result.find_first_not_of(whitespace, beginSpace);
        const auto range = endSpace - beginSpace;
        
        result.replace(beginSpace, range, fill);
        
        const auto newStart = beginSpace + fill.length();
        beginSpace = result.find_first_of(whitespace, newStart);
    }
    
    return result;
}


class Instr
{
public:
    uint offset;
    uint size;
    string line;
    Line *lineClass;
    Byte *bytes;
    Instr& setOpcode(Byte opcode) { bytes[size++] = opcode; return *this;};
    Instr& addI8(Byte param) { bytes[size++] = param;  return *this;}
    Instr& addI16(uint16_t param) {
#ifdef YSC
        bytes[size++] = param & 0xFF;
        bytes[size++] = (param >> 8) & 0xFF;

#else
        bytes[size++] = (param >> 8) & 0xFF;
        bytes[size++] = param & 0xFF;
#endif
        return *this;
    }
    Instr& addSI16(int16_t param) {
#ifdef YSC
        bytes[size++] = param & 0xFF;
        bytes[size++] = (param >> 8) & 0xFF;
        
#else
        bytes[size++] = (param >> 8) & 0xFF;
        bytes[size++] = param & 0xFF;
#endif
        return *this;}
    
    Instr& addI24(uint32_t param) {
#ifdef YSC
        bytes[size++] = param & 0xFF;
        bytes[size++] = (param >> 8) & 0xFF;
        bytes[size++] = (param >> 16) & 0xFF;


#else
        bytes[size++] = (param >> 16) & 0xFF;
        bytes[size++] = (param >> 8) & 0xFF;
        bytes[size++] = param & 0xFF;
#endif
        return *this;
    }
    Instr& addI32(uint32_t param) {
#ifdef YSC
                bytes[size++] = param & 0xFF;
                bytes[size++] = (param >> 8) & 0xFF;
                bytes[size++] = (param >> 16) & 0xFF;
                bytes[size++] = (param >> 24) & 0xFF;



#else
        bytes[size++] = (param >> 24) & 0xFF;
        bytes[size++] = (param >> 16) & 0xFF;
        bytes[size++] = (param >> 8) & 0xFF;
        bytes[size++] = param & 0xFF;
#endif
        return *this;
    }
    
    Instr(int numBytes=32) { bytes = new Byte[numBytes]; memset(bytes, 0, numBytes); size = 0; offset = 0;}
    ~Instr() { delete[] bytes; }
    
};


class JumpInstr : public Instr
{
public:
    string jumpLabel;
    bool resolved;
    JumpInstr(string jumpLabel) {
        this->jumpLabel = jumpLabel;
        resolved = false;
        size = 0;
    }
};




class CallInstr : public Instr
{
public:
    string callLabel;
    bool resolved;
    CallInstr(string callLabel) : Instr() {
        this->callLabel = callLabel;
        resolved = false;
                Instr::setOpcode(93);
         Instr::size=4;

    }
};


class PushFuncInstr : public Instr
{
public:
    string callLabel;
    bool resolved;
    PushFuncInstr(string callLabel) {
        this->callLabel = callLabel;
        resolved = false;
        size=4;
        setOpcode(40);
    }
};


class Label : public Instr
{
    
    public:
    Label() { setOpcode(129); size = 0; }
    string label;

};
class Module;
class Function : public Instr
{
private:
    uint32_t currentOffset;
    vector<Instr*> instructions;
    vector<CallInstr*> unresolvedSymbols;
public:
    string callLabel;
    uint32_t paramCount;
    uint32_t localCount;
    Module *parentModule;
    uint32_t type;
    uint32_t startOffset;
    uint32_t endOffset;
    vector<Label> labels;
    
    void addLabel(string label, uint32_t offset)
    {
//        for(int i=0; i<labels.size(); i++)
//        {
//            if(labels[i].label == label && labels[i].offset == offset)
//                return;
//        }
//        Label lab;
//        lab.label = label;
//        lab.offset = offset;
//        labels.push_back(lab);
        Label *lab = new Label();
        lab->label = label;
        addInstruction(lab);
        //labels.push_back(*lab);
    }
    void addLabel(string label)
    {
        Label *lab = new Label();
        lab->label = label;
        addInstruction(lab);
        //labels.push_back(*lab);
        
    }
    void addInstruction(Instr* instruction)
    {
        if(instructions.size() == 0)
            instruction->offset = 0;
        else
        {
            instruction->offset = currentOffset;
        }
        
        currentOffset += instruction->size;
        instructions.push_back(instruction);
    }
    void addInstruction(CallInstr *instruction)
    {
        unresolvedSymbols.push_back(instruction);
        this->addInstruction((Instr*)instruction);
    }
    
    void dump() {
        for(Instr *I : instructions) {
            if(I->lineClass && I->size > 0) {
                if(I->offset < 10)
                    cout << "( " << I->offset << ")";
                else
                    cout << "("<<I->offset << ")";
                cout << I->lineClass->tokens[0] << " ";
                for(int i=1; i<I->size; i++)
                    cout << (uint32_t)I->bytes[i] << " ";
                
                cout << endl;
            }
        }
    }
    Function() { currentOffset = startOffset = endOffset = type = 0; }
    vector<Instr*>& getInstructions() { return instructions; }
};
class Module
{
public:
    vector<Function*> functions;
    vector<string> natives;
    vector<string> stringTable;
    void dump() {
        for(Function *F : functions) {
            F->dump();
        }
    }
};

class CallNative : public Instr
{
public:
    string nativeName;
    bool resolved;
    uint16_t index = -1;
    CallNative(Module &m, string *callLabel, int paramCount, int returnCount) : Instr() {
        
        nativeName = *callLabel;
        for(int i=0; i<m.natives.size(); i++) {
            if(m.natives[i] == *callLabel) {
                resolved = true;
                index = i;
            }
        }
        if(!resolved) {
            index = m.natives.size();
            m.natives.push_back(*callLabel);
            resolved = true;
        }
        setOpcode(44).addI8((returnCount&0x3) | (paramCount << 2)).addI16(CFSwapInt16(index))
        ;
    }
};

void fixOffsets(Module &m) {
    uint32_t offset = 0;
    for(Function *f : m.functions) {
        f->startOffset = offset;
        uint32_t instOffset = 0;
        for(Instr *I : f->getInstructions()) {
            offset += I->size;
            I->offset = instOffset;
            instOffset+=I->size;
        }
        f->endOffset = offset;
    }
}

void decodeInstruction(Line &line, Function *f)
{
    string op = line.tokens[0];
    Instr *inst = new Instr();
    inst->line = line.orig;
    inst->lineClass = &line;
    if(isComment(op))
        return;
    if(op.c_str()[0] == ':')
    {
        f->addLabel(line.tokens[0]);
        return;
    }
    else if(op == "Call" || op == "call")
    {
        assert(line.tokens.size() >= 2);
        {
            CallInstr *call = new CallInstr(line.tokens[1]);
            f->addInstruction(call);
            call->line = line.orig;
            call->lineClass = &line;
            //inst = call;
            return;
        }
    }
    else if(op == "CallNative" || op == "callnative")
    {
        assert(line.tokens.size() >= 2);
        {
            string substr = line.tokens[1].substr(1, line.tokens[1].length()-1);
            
            CallNative *call = new CallNative(*f->parentModule, &substr, stoi(line.tokens[2].c_str()), stoi(line.tokens[3].c_str()));
            f->addInstruction(call);
            call->line = line.orig;
            call->lineClass = &line;
            //inst = call;
            return;
        }
    }
    else if(op == "SwitchStart") {
        inst->lineClass->tokens[0] = "Switch";
        inst->setOpcode(230);
        inst->addI8(0);
        
    }
    else if(op == "ArraySet") {
        inst->lineClass = &line;
         uint32_t val = atoi(line.tokens[1].c_str());
        if((val & 0xFF) == val){
            inst->setOpcode(54).addI8(val);
        } else if((val & 0xFFFF) == val) {
            inst->setOpcode(75).addI16(val);
        } else {
            assert(!"unsupported instruction");
        }
    }
    else if(op == "SwitchEnd") {
        inst->setOpcode(232);
        inst->size = 0;
    } else if(op == "case") {
        inst->setOpcode(231);
        inst->size = 0;
    } else if(op == "fPush") {
        float val = atof(line.tokens[1].c_str());
        
        if(((int32_t)val) == val) {
            if(val >= -1 && val <= 7) { // const push
                inst->setOpcode(val+119);
                inst->lineClass->tokens[0] = "fPush_" + to_string((uint32_t)val);
            }
        } else {
            uint32_t *temp = (uint32_t*)((void*)&val);
            inst->setOpcode(41).addI32(*temp);
        }
    }
    else if(op == "iPush" || op == "ipush")
    {
        if(line.tokens.size() == 4) // Push I8I8I8
        {
            inst->setOpcode(39) \
            .addI8(u8imm(line.tokens[1])) \
            .addI8(u8imm(line.tokens[2])) \
            .addI8(u8imm(line.tokens[3]));

        }
        else if(line.tokens.size() == 3) // Push I8I8
        {
            inst->setOpcode(38) \
            .addI8(u8imm(line.tokens[1])) \
            .addI8(u8imm(line.tokens[2]));
        }
        else if(line.tokens.size() == 2)
        {
            //Push
            if(line.tokens[1].find(".") != 0)
            {
             
                int64_t val = atoll(line.tokens[1].c_str());
                
                if(val >= -1 && val <= 7) { // const push
                    inst->setOpcode(val+110);
                    inst->lineClass->tokens[0] = "iPush_" + to_string(val);
                } else if((val & 0xFF) == val) // 8 bit push
                    inst->setOpcode(37).addI8(u8imm(line.tokens[1]));
                else if((val & 0xFFFFFFFF) == val) // 32 bit push
                    inst->setOpcode(40).addI32(u32imm(line.tokens[1]));
                else
                    assert(!"64 bit types not supported");
            
            }
            else
            {
                inst->setOpcode(41).addI32(atof(line.tokens[1].c_str()));
                //float
            }
        }

        else
            assert(!"Unhandled Type");
    }
    else if(op == "Jump")
    {
        JumpInstr *jmp = new JumpInstr(line.tokens[1]);
        jmp->setOpcode(85);
        jmp->line = line.orig;
        jmp->lineClass = &line;
        jmp->size =3;
        f->addInstruction(jmp);

        return;
    }
    else if(op == "JumpFalse")
    {
        JumpInstr *jmp = new JumpInstr(line.tokens[1]);
        jmp->setOpcode(86);
        jmp->line = line.orig;
        jmp->lineClass = &line;
        jmp->size = 3;
        f->addInstruction(jmp);

        return;
    }
    else if(op == "JumpTrue")
    {
        JumpInstr *jmp = new JumpInstr(line.tokens[1]);
        jmp->setOpcode(88);
        jmp->line = line.orig;
        jmp->lineClass = &line;
        jmp->size = 3;
        f->addInstruction(jmp);

        return;
    }
    else if(op == "function" || op == "Function") {
        inst->setOpcode(0).addI32(0);
    }
    else if(op == "scpy") {
        inst->setOpcode(101).addI8(atoi(line.tokens[1].c_str()));
    } else if(op == "pSet") {
        inst->setOpcode(48);
    } else if(op == "ArrayGet") {
        inst->lineClass = &line;
        uint32_t val = atoi(line.tokens[1].c_str());
        if((val & 0xFF) == val){
            inst->setOpcode(53).addI8(val);
        } else if((val & 0xFFFF) == val) {
            inst->setOpcode(74).addI16(val);
        } else {
            assert(!"unsupported instruction");
        }
    }
    else if(op == "not")
        inst->setOpcode(6);
    else if(op == "icmpne")
        inst->setOpcode(9);
    else if(op == "icmpeq")
        inst->setOpcode(8);
    else if(op == "XOR")
        inst->setOpcode(33);
    else if(op == "add" || op=="addi")
        inst->setOpcode(1);
    else if(op == "sub" || op == "isub")
        inst->setOpcode(2);
    else if(op == "dup")
        inst->setOpcode(42);
    else if(op == "itof")
        inst->setOpcode(34);
    else if(op == "iand")
        inst->setOpcode(31);
    else if(op == "or")
        inst->setOpcode(32);
    else if(op == "drop")
        inst->setOpcode(43);
    else if(op == "pcall" || op == "pCall")
        inst->setOpcode(108);
    else if(op == "icmplt")
        inst->setOpcode(12);
    else if(op == "Return" || op == "return")
        inst->setOpcode(46).addI8(u8imm(line.tokens[1])).addI8(u8imm(line.tokens[2]));
    else if(op == "ArrayGetP") {
                int64_t val = atoll(line.tokens[1].c_str());
        if((val & 0xFF) == val) // 8 bit push
            inst->setOpcode(52).addI8(u8imm(line.tokens[1]));
        else if((val & 0xFFFF) == val) // 16 bit push
            inst->setOpcode(73).addI16(u16imm(line.tokens[1]));
        
        else
            assert(!"64 bit types not supported");
    }else if(op == "pFrame1" || op == "pFrame2")
    {
        int64_t val = atoll(line.tokens[1].c_str());
        
        if((val & 0xFF) == val) // 8 bit push
            inst->setOpcode(55).addI8(u8imm(line.tokens[1]));
        else if((val & 0xFFFF) == val) // 16 bit push
            inst->setOpcode(76).addI16(u16imm(line.tokens[1]));
    
        else
            assert(!"64 bit types not supported");
    }
    else if(op == "PushString") {
        line.tokens[1] = line.tokens[1].substr(1, line.tokens[1].length()-2);
        int ind = 0;
        bool found = false;
        for(int i=0; i<f->parentModule->stringTable.size(); i++) {
            if(f->parentModule->stringTable[i] == line.tokens[1]) {
                found = true;
                break;
            }
            ind += f->parentModule->stringTable[i].length()+1;
        }
        if(!found)
            f->parentModule->stringTable.push_back(line.tokens[1]);
        
        //ind = f->parentModule->stringTable.size()-1;
        inst->setOpcode(40).addI32(ind).addI8(99);
    } else if(op == "StaticGet1")
        inst->setOpcode(59).addI8(u8imm(line.tokens[1]));
    else if(op == "StaticSet1")
        inst->setOpcode(60).addI8(u8imm(line.tokens[1]));
    else if(op == "StaticGet2")
        inst->setOpcode(80).addI16(u16imm(line.tokens[1]));
    else if(op == "StaticSet2")
        inst->setOpcode(81).addI16(u16imm(line.tokens[1]));
    else if(op == "StaticGetP") {
        uint32_t val = u32imm(line.tokens[1]);
        if((val&0xFF) == val)
            inst->setOpcode(58).addI8(u8imm(line.tokens[1]));
        else if((val&0xFFFF) == val)
            inst->setOpcode(79).addI16(u16imm(line.tokens[1]));
        else
            assert(!"StaticGetP Index too large");
    }
    else if(op == "SetImm1")
        inst->setOpcode(66).addI8(u8imm(line.tokens[1]));
    else if(op == "SetImm2")
        inst->setOpcode(72).addI16(u16imm(line.tokens[1]));
    else if(op == "GetImm1")
        inst->setOpcode(65).addI8(u8imm(line.tokens[1]));
    else if(op == "GetImm2")
        inst->setOpcode(71).addI16(u16imm(line.tokens[1]));
    else if(op == "getFrame1" || op == "GetFrame1")
        inst->setOpcode(56).addI8(u8imm(line.tokens[1]));
    else if(op == "getFrame2" || op == "GetFrame2")
        inst->setOpcode(77).addI16(u16imm(line.tokens[1]));
    else if(op == "setFrame1" || op == "SetFrame1")
        inst->setOpcode(57).addI8(u8imm(line.tokens[1]));
    else if(op == "setFrame2" || op == "SetFrame2")
        inst->setOpcode(78).addI16(u16imm(line.tokens[1]));
    else if(op == "GlobalSet3")
        inst->setOpcode(96).addI24(u24imm(line.tokens[1]));
    else if(op == "GlobalGet3")
        inst->setOpcode(95).addI24(u24imm(line.tokens[1]));
    else if(op == "GlobalGetp")
        inst->setOpcode(94).addI24(u24imm(line.tokens[1]));
    else if(op == "GetImmP" || op == "GetImmp") {
        uint32_t val = u32imm(line.tokens[1]);
        if((val&0xFF) == val)
            inst->setOpcode(64).addI8(u8imm(line.tokens[1]));
        else if((val&0xFFFF) == val)
            inst->setOpcode(78).addI16(u16imm(line.tokens[1]));
        else
            assert(!"GetImmP Index too large");
        
    } else if(op == "PushFunction")
    {
        assert(line.tokens.size() >= 2);
        {
            PushFuncInstr *call = new PushFuncInstr(line.tokens[1]);
            f->addInstruction(call);
            call->line = line.orig;
            call->lineClass = &line;
            return;
        }
    }
    else
        cout << "Unhandled line : " << line.tokens[0] << endl;
    if(inst->size != 0 || (inst->bytes[0] >= 230 && inst->bytes[0] <= 232))
        f->addInstruction(inst);
}

bool isComment(string str) {
    return (str[0] == '/' && str[1] == '/');
}

//void getFunctionOffset(string label, Module &m) {
//    for(Function *f : m.functions) {
//        if(f->callLabel == label) {
//            
//        }
//    }
//}

struct caseStruct {
    uint32_t val;
    uint16_t offset;
};
void processSwitch(Instr *instr, int *index, Function *func) {
    int numCases = 0;
    vector<caseStruct> cases;
    for(;(*index)<func->getInstructions().size(); (*index)++) {
        Instr *i = func->getInstructions()[*index];
        
        if(i->bytes[0] == 230) {
//            ++index;
            processSwitch(i, &(++(*index)), func);
        }
        else if(i->bytes[0] == 231) {
            // handle case
            numCases++;
            uint32_t *val = NULL;
            float f;
            int d;
            if(i->lineClass->tokens[1].find(".") == 0) {
                f = stof(i->lineClass->tokens[1].c_str()+1);
                val = (uint32_t*)((void*)&f);
            } else {
                d = stoi(i->lineClass->tokens[1].c_str()+1);
                val = (uint32_t*)((void*)&d);
            }
//            instr->bytes[1]++;
//            instr->addI32(*val);
//            instr->addI16(i->offset);
            caseStruct s;
            s.offset = i->offset;
            s.val = *val;
            cases.push_back(s);
        } else if(i->bytes[0] == 232) {
            // handle switch end
            for(int j=0; j<cases.size(); j++) {
                instr->bytes[1]++;
                instr->addI32(cases[j].val);
                instr->addI16((cases[j].offset - (instr->offset) + (cases.size()*6)));
            }
        }
    }
}
int main(int argc, const char * argv[])
{

    // insert code here...
    ifstream in(argv[1]);
    vector<string> lines;
    vector<Line*> linesSplit;
    if(in.is_open())
    {
        while(!in.eof())
        {
            char line[2048];
            in.getline(line, 2048);
            string l = string(line);
            if(line[0] == '#' || (line[0] == '/' && line[1] == '/') || line[0] == 0 || l == " ")
                continue;
            lines.push_back(reduce(l));
        }
        
        
        //bool shouldBreak=false;
        for(int i=0; i<lines.size(); i++)
        {
            string line = lines[i];
            std::istringstream iss(line);
            std::string token;
            Line *li = new Line;
            li->orig = line;
            while(getline(iss, token, ' '))
            {
                if(token[0] == '/' && token[1] =='/') {
                    break;
                }
                li->tokens.push_back(token);
            }
            if(line[0] == ':' || isComment(line)) {
                li->tokens.push_back(line);
            }
            linesSplit.push_back(li);
        }
        Module m;
        Function *cFunction = NULL;
        
        for(int i=0; i<linesSplit.size(); i++)
        {
            Line *l = linesSplit[i];
            if(isComment(l->orig))
                continue;
            if(l->tokens[0] == "Function")
            {

                
                assert((linesSplit[++i]->tokens[0] == "{"));


                Function *f = new Function;
                f->parentModule = &m;
                if(m.functions.size() == 0)
                    f->startOffset = 0;
                else
                    f->startOffset = (m.functions[m.functions.size()-1])->endOffset;
                f->paramCount = u32imm(l->tokens[2]);
                f->callLabel = l->tokens[3];
                cFunction = f;
                m.functions.push_back(f);
                                decodeInstruction(*l, cFunction);
                while(linesSplit[++i]->tokens[0] != "}")
                {
                   Line *line = linesSplit[i];
                    decodeInstruction(*line, cFunction);
                    
                }
                
                if(linesSplit[i]->tokens[0] == "}") {
                    f->localCount = u32imm(linesSplit[i]->tokens[3]);
                }
                
                vector<Instr*> &instructions = cFunction->getInstructions();
                
                cFunction->endOffset = cFunction->startOffset + instructions[instructions.size()-1]->offset+instructions[instructions.size()-1]->size;
                continue;
            }
        }
        
        for(int i=0; i<m.functions.size(); i++) {
            Function *f = m.functions[i];
            if(f->callLabel == "@main") {
                if(i == 0)
                    break;
                else {
                    Function *mainFunction = f;
                    m.functions.erase(m.functions.begin()+i);
                    m.functions.insert(m.functions.begin(), mainFunction);
                    break;
                    
                    
                }
            }
        }
        

        fixOffsets(m);
        
        for(Function *f : m.functions) {
            for(int i=0; i<f->getInstructions().size(); i++) {
                Instr *instr = f->getInstructions()[i];
                if(instr->bytes[0] == 230) {
                    processSwitch(instr, &(++i), f);
                }
            }
        }
        
        fixOffsets(m);
        
                YSCHeader header;
        for(Function *f : m.functions) {
//            if(f->localCount > header.localsCount)
//                header.statics_size = f->localCount;
            
            for(int i=0; i<f->getInstructions().size(); i++) {
                Instr *instr = f->getInstructions()[i];
                if(instr->bytes[0] == 93) {
                    CallInstr *call = (CallInstr*)instr;
                    if(call->resolved == false) {
                        for(Function *t : m.functions) {
                            if(t->callLabel == call->callLabel) {
                                call->size = 1;
                                call->addI24(t->startOffset);
                                call->resolved = true;
                                break;
                            }
                        }
                    }
                }  else if(instr->bytes[0] == 85 || instr->bytes[0] == 86 || instr->bytes[0] == 88) {
                    JumpInstr *jump = (JumpInstr*)instr;
                    for(int i=0; i<f->getInstructions().size(); i++) {
                        Instr *I = f->getInstructions()[i];
                        if(I->bytes[0] == 129) {
                            Label *label = (Label*)I;
                            if(strcmp(label->label.c_str()+1, jump->jumpLabel.c_str()+1)==0) {
                                jump->size = 1;
                                int destOffset = f->getInstructions()[i+1]->offset;
                                int sourceOffset = jump->offset;

                                    sourceOffset += 3;
                                jump->addSI16(destOffset-sourceOffset);
                                continue;
                            }
                        }
                    }
                    
                } else if(instr->bytes[0] == 0) {
                    //inst->setOpcode(40).addI32(u32imm(line.tokens[1]));
                    for(Function *t : m.functions) {
                        
                        if(instr->lineClass->tokens[0] != "PushFunction")
                            continue;
                        PushFuncInstr *call = (PushFuncInstr*)instr;
                        
                        
                        if(t->callLabel == call->callLabel) {
                            call->size = 0;
                            instr->setOpcode(40).addI32(t->startOffset);
                            call->resolved = true;
                            break;
                        }
                    }
                }
            }
        }
        


        header.code_size = m.functions.back()->endOffset;
        NSMutableData *code = [[NSMutableData alloc] init];
        for(Function *f : m.functions) {
            uint32_t temp = 45;
            [code appendBytes:&temp length:1];
            temp = f->paramCount;
            [code appendBytes:&temp length:1];
#ifdef YSC
            temp = f->localCount;
#else
            temp = CFSwapInt16(f->localCount);
#endif
            [code appendBytes:&temp length:2];
            temp = 0;
            [code appendBytes:&temp length:1];
            
            for(Instr *I : f->getInstructions()) {
                if(I->bytes[0] == 0 || I->size == 0)
                    continue;
                if(I->bytes[0] == 230) {
                    I->bytes[0] = 98;
                    [code appendBytes:I->bytes length:I->size];
                    continue;
                }
                [code appendBytes:I->bytes length:1];
                if(I->size > 1) {
                     if(I->size == 2)
                        [code appendBytes:I->bytes+1 length:1];
                    else if(I->size == 3) {
                        temp = *(uint16_t*)(I->bytes+1);
                        //temp = CFSwapInt16(temp&0xFF);
                        [code appendBytes:&temp length:2];
                    } else if(I->size == 4) {
                        temp = *(uint32_t*)(I->bytes+1);
                        //temp = CFSwapInt16(temp&0x00FFFFFF);
                        [code appendBytes:&temp length:3];
                    } else if(I->size == 5) {
                        temp = *(uint32_t*)(I->bytes+1);
                        //temp = CFSwapInt32(temp);
                        [code appendBytes:&temp length:4];
                    } else if(I->size == 6) {
                        temp = *(uint32_t*)(I->bytes+1);
                        [code appendBytes:&temp length:4];
                        [code appendBytes:I->bytes+5 length:1];
                    }else {
                        cout << "WTF?" << endl;
                    }
                }
            }
        }
        header.natives_size = m.natives.size();
        
        UInt64 *natives = new UInt64[m.natives.size()*8];
        header.natives = natives;
        header.code = (uint64_t*)code.bytes;

        for(int i=0; i<m.natives.size(); i++) {

            string str = m.natives[i];
            NSString *ocString = [[NSString alloc] initWithUTF8String:str.c_str()];
            NSNumber *hash = nil;//joaat(buffer, m.natives[i].size());
            if([ocString rangeOfString:@"unk_0x"].length != 0) {
                
                NSString *hexVal = [[ocString substringFromIndex:4] uppercaseString];
                
                if(hexVal.length < 11) {
                    NSDictionary *jhash = jTables();
                    NSScanner* pScanner = [NSScanner scannerWithString:hexVal];
                    
                    uint64_t iValue;
                    //[pScanner scanHexInt: &iValue];
                    [pScanner scanHexLongLong:&iValue];
                    hash = @(iValue);
                    

                    hash = jhash[hash];
                } else {
                    hexVal = [hexVal substringFromIndex:2];
                    
                    for(NSDictionary *dic in tTables()) {
                        NSString *newHash = dic[hexVal];
                        if(newHash != nil)
                            hexVal = newHash;
                    }
                    NSScanner* pScanner = [NSScanner scannerWithString:hexVal];
                    
                    uint64_t iValue;
                    //[pScanner scanHexInt: &iValue];
                    [pScanner scanHexLongLong:&iValue];
                    hash = @(iValue);
                }
            } else {

            NSDictionary *nTable = nativeTable();
            for(NSString *str in nTable.allValues) {
                if([str.lowercaseString isEqualToString:ocString.lowercaseString]) {
                    hash = [[nTable allKeysForObject:str] lastObject];
                }
            }
            }
            assert((hash!=nil));
            natives[i] = [hash unsignedLongLongValue];
            natives[i] = rotr(natives[i], header.code_size+i);
            
        }
        
        
        int size = 0;
        for(int i=0; i<m.stringTable.size(); i++) {
            size += m.stringTable[i].size()+1;
        }
        
        char *strings = new char[size];
        size = 0;
        for(int i=0; i<m.stringTable.size(); i++) {
            strcpy(strings+size, m.stringTable[i].c_str());
            size += m.stringTable[i].length()+1;
        }
        header.string_table_size = size;
        header.strings = (uint8_t*)strings;
        
        uint8_t *scFile = header.createFile();
        NSData *data = [NSData dataWithBytes:scFile length:header.getFileSize()];
        [data writeToFile:@"patched_charanimtest.ysc" atomically:TRUE];

        m.dump();
        wait(0);
    }
    return 0;
}

