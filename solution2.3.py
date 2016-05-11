# based on solution2.2.py
# use dictionary for the search

class state:
    ''' object: state '''
    def __init__(self,parent,moveblock,movedirection,mode):
        ''' create a state with mode'''
        self.parent=parent
        self.moveblock=moveblock
        self.movedirection=movedirection
        self.mode=mode
        self.blocks=[]
        self.nblocks=0
        for block in self.mode:
            #if block != 0 :
            if block :
                if block not in self.blocks:
                    self.blocks.append(block)
                    self.nblocks+=1

    def move(self,block,direction):
        ''' move a block.
        Direction, up 1; down -1; left -1; right 1
        '''
        # block occupies at
        pos=[i for i, j in enumerate(self.mode) if j == block ]
#        print 'pos', pos
        new=self.mode[:]

        # determine the block type, vertical or horizon
        if block%2 == 0:
            # can move only vertically
            if direction == 1:  # move up
                top=pos[0]-6   # 6 is the dimension
                if top >= 0 and new[top] == 0 :
                    new[top] = block
                    new[pos[-1]] = 0
#                    print 'move up'
                    return new
            else:               # move down
                bottom=pos[-1]+6
                if bottom <= 35 and new[bottom] == 0:
                    new[bottom]=block
                    new[pos[0]] = 0
#                    print 'move down'
                    return new
        else:
            # can only move horizonly
            if direction == 1:  # move up
                left=pos[0]-1   # 6 is the dimension
                if left >= 0 and pos[0]%6 != 0 and  new[left] == 0 :
                    new[left] = block
                    new[pos[-1]] = 0
#                    print 'move left'
                    return new
            else:               # move down
                right=pos[-1]+1
                if right <= 35 and right%6 !=0 and new[right] == 0 :
                    new[right]=block
                    new[pos[0]] = 0
#                    print 'move right'
                    return new
        return False

class library():
    ''' library to store instance'''
    def __init__(self):
        self.lib=[]
        self.dic={}

    def AddItem(self,instance):
        ''' add to dead library, the instance is new'''
        self.lib.append(instance)
        key=''.join([str(i) for i in instance.mode])
        self.dic[key]=instance

#   def DeleteItem(self,mode):
#       ''' Delete the instance that match the given mode'''
#       for item in self.lib:
#           if item.mode ==  mode:
#               self.lib.pop(self.lib.index(item))

    def Inlib(self,mode):
        ''' Check if the instance is in the library. '''
        for item in self.lib:
            if mode == item.mode:
                return True
        return False

    def Indic(self,mode):
        ''' check if in the dic '''
        key=''.join([str(i) for i in mode])
        return key in self.dic

    def print_lib(self):
        for item in self.lib:
            print item.mode


def start(initial):
    def parent_of(mode):
        ''' Find the oldest parent object of a certain mode'''
        #first=True
        key=''.join([str(i) for i in mode])
        return all_lib.dic[key].parent
#       for item in all_lib.lib[:]:
#           if item.mode == mode:
#               candidate=item
#       return

    def search(instance,search_lib,all_lib):
        ''' generate offsprings and add to search lib and all lib '''
        if  (instance.mode[17] == 0) and (instance.mode[16] == 1) and (instance.mode[15] == 1) :
            print "Found"
            return instance
        # move block if can, and compare
        for block in instance.blocks:
            for direction in [1,-1]:
                newmode=instance.move(block,direction)
                #if not all_lib.Inlib(newmode):
                #if not all_lib.Indic(newmode):
                if newmode and not all_lib.Indic(newmode):
                    new=state(instance,block,direction,newmode)
                    search_lib.AddItem(new)
                    all_lib.AddItem(new)
        return False

    search_lib=library()
    all_lib=library()

    new=state(None,None,None,initial)
    search_lib.AddItem(new)
    all_lib.AddItem(new)

    i=0
    resultinst = False
    while resultinst == False:
        resultinst=search(search_lib.lib[i], search_lib, all_lib)
        i+=1
        if i % 1000 == 0 :
            print i
            print 'len of search lib', len(search_lib.lib)
            print '   len of all lib', len(all_lib.lib)
            print '======================='

    print i
    print 'len of search lib', len(search_lib.lib)
    print '   len of all lib', len(all_lib.lib)
    print '==  END  =============='

    solut=[]
    # find the route
    mode=resultinst.mode

    solut.append(mode)
    moveblock=resultinst.moveblock
    movedirection=resultinst.movedirection

    while mode != None:
        temp=parent_of(mode)
        if temp != None:
            moveblocknew=temp.moveblock
            movedirectionnew=temp.movedirection
            if (moveblocknew == moveblock) and (movedirectionnew == movedirection):
                mode=temp.mode
                print 'skip'
            else:
                mode=temp.mode
                solut.append(mode)
                moveblock=moveblocknew
                movedirection=movedirectionnew
        else:
            mode=None

    solut.reverse()

    print "One doable route:"
    for mode in solut:
        print '%3d%3d%3d%3d%3d%3d' % (mode[0], mode[1], mode[2], mode[3], mode[4], mode[5] )
        print '%3d%3d%3d%3d%3d%3d' % (mode[6], mode[7], mode[8], mode[9], mode[10],mode[11])
        print '%3d%3d%3d%3d%3d%3d' % (mode[12],mode[13],mode[14],mode[15],mode[16],mode[17])
        print '%3d%3d%3d%3d%3d%3d' % (mode[18],mode[19],mode[20],mode[21],mode[22],mode[23])
        print '%3d%3d%3d%3d%3d%3d' % (mode[24],mode[25],mode[26],mode[27],mode[28],mode[29])
        print '%3d%3d%3d%3d%3d%3d' % (mode[30],mode[31],mode[32],mode[33],mode[34],mode[35])
        print '==================='

    print "Route printed"
    print 'Route steps:', len(solut)

#level 1
initial1=[
 0, 0, 2, 0, 0, 0,
 0, 0, 2, 0, 0, 0,
 1, 1, 2, 4, 0, 6,
 0, 0, 0, 4, 0, 6,
 0, 0, 0, 4, 0, 0,
 0, 3, 3, 5, 5, 0]

# level easy
initial= [
 0, 0, 0, 4, 0, 0,
 3, 3, 3, 4, 0, 0,
 1, 1, 2, 4, 0, 6,
 0, 0, 2, 7, 7, 6,
 5, 5, 5, 0, 0, 8,
 0, 0, 0, 0, 0, 8]

#level 74
initial74=[
 2, 0, 5, 5, 9, 9,
 2, 0, 0, 6,11,11,
 1, 1, 4, 6,10, 0,
 3, 3, 4, 8,10,12,
 7, 7, 7, 8,10,12,
 0, 0, 0,13,13,13]

# level 94
initial94=[
 2, 4, 7, 7,10,12,
 2, 4, 0, 8,10,12,
 2, 1, 1, 8, 0,12,
 0, 3, 3, 0, 9, 9,
 0, 0, 6, 0,11,11,
 5, 5, 6,13,13, 0]

start(initial94)

