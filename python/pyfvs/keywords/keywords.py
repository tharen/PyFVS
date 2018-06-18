"""
Define FVS keywords as Python classes

Keywords are subclasses of KeywordBase. KeywordBase enforces field ordering
default field valeus, data type casting, field width, etc. 

"""

# #TODO: Need an expression object to represent COMPUTE children records

import time
import datetime
import logging
import types
import collections

# import the parent class module
from ._fields import *
from . import eventmonitor

from six import with_metaclass

# initiate logging as prepared by fvs._utils
log = logging.getLogger('pyfvs.keywords.keywords')

# The most simple keyword is used as a flag in FVS and merely needs to subclass
# the Keyword parent class and provide a mnemonic (FVS keyword string)
# field formatting is defined by defining an implemetation of one of the
# KeywordField subclasses.  Arguments to the subclass specify the alignment,
# width, precision, and default value

# Keyword format constants
# KW_FMT_SINGLE = 0         #single line keyword format
# KW_FMT_SUPPLEMENTAL = 1   #single supplemental data line keyword format
# KW_FMT_MULTI_LINE = 2     #multiple supplemental data line keyword format

KW_FMT_ONELINE = 0
KW_FMT_SUPPLEMENTAL = 1
KW_FMT_STACKED = 2
KW_FMT_ONELINE_BLOCK = 3
KW_FMT_PARMS = 4
KW_FMT_ONELINE_STACKED = 5

class KeywordMetaClass(type):
    """
    Metaclass magic for KeywordBase
    """
    # TODO: Test this with Python 2.6+
    @classmethod
    def __prepare__(self, *args, **kwargs):
        # Use an OrderedDict to keep the attributes in the defined order
        return collections.OrderedDict()

    def __new__(mcl, name, bases, attrs):
        # generate a list of field names from the OrderedDict
        flds = [k for k in attrs.keys()
                if isinstance(attrs[k], KeywordFieldBase)]

        attrs['__fields__'] = flds

        new_inst = super(
                KeywordMetaClass, mcl).__new__(mcl, name, bases, attrs)

        return new_inst

# NOTE: Python 3 introduced the metaclass keyword argument
# TODO: Revert back to create an equivalent for Python 2.6+
class KeywordBase(with_metaclass(KeywordMetaClass, object)):
    """
    Base class of an FVS keyword represented as a list of 7 fields
    
    Fields are defined on subclasses using one of the Field classes.  Field 
        order is enforced as the order of definition during class creation.  
        
    During instantiation, fields defined by subclasses are copied to the 
        instance dictionary, self.__dict__, to avoid namespace pollution.  
        
    To designate fields as supplemental records put their attribute name in the
        __supplemental__ class attribute.
    
    :param mnemonic: FVS shortname for the keyword
    :param name: Long name for the keyword
    :param children: Nested keywords for multipart keywords
    :param rank: Precedence in the key file
    :param format: Key file format
            0 - Single line, <= 7 data fields
            1 - Supplemental data is on a second line
            2 - Supplemental data on multiple lines, one per valid field
            3 - Block keyword with children keywords and END statement
            4 - PARMS keyword format, fields can be expressions, constants, or values
            5 - Single line with children (supplemental records, etc.)
    :param comment: single line comment string
    """

    # TODO: Refine keyword formatting options
    # TODO: Implement PARMS as and alternative to field formatting
    # TODO: Format 0 keywords should include an optional parms_fmt argument
    #       to automatically handle PARMS compatible keywords.

    #__metaclass__ = KeywordMetaClass
    __fields__ = ()
    __supplemental__ = ()

    def __new__(cls, *args, **kargs):
        """
        Manipulate the creation of Keyword instances
        """
        # #TODO: field ordering was move to the metaclass
#        new_inst = super(KeywordBase, cls).__new__(cls, *args, **kargs)
        new_inst = super(KeywordBase, cls).__new__(cls,)


        for f in cls.__fields__:
            fld = copy.deepcopy(getattr(cls, f))
            super(KeywordBase, new_inst).__setattr__(f, fld)
#            setattr(new_inst, f, None)

        return new_inst

    def __init__(self, mnemonic, name='', children=[], rank=1, format=0
            , add_vals=[], end_block=False, comment=''
            , **kargs):
        self.mnemonic = mnemonic
        self.name = name
        self.children = children[:]
        self.rank = rank
        self.format = format
        self.end_block = end_block
        self.comment = comment

#        for f in self.__fields__:
#            print 'Field init', self.__dict__[f].name

        self.add_vals = []
        for i, val in enumerate(add_vals):
            fn = 'fld_ % d' % i
            setattr(self, fn, val)
            self.add_vals.append(fn)

    def __getattribute__(self, attr):
        """
        Return the data value of keyword fields
        """
        # attributes in the __fields__ tuple return their "value" attribute
        if attr in  object.__getattribute__(self, '__fields__'):
            v = self.__dict__[attr].value
            return v

        # regular attributes return their value as usual
        else:
            v = super(KeywordBase, self).__getattribute__(attr)
            return  v

    def __setattr__(self, attr, value):
        """
        Override to handle setting "Field" values 
        """
        if attr in object.__getattribute__(self, '__fields__'):
            fld = super(KeywordBase, self).__getattribute__(attr)
            setattr(fld, 'value', value)

        # regular attributes are set as usual
        else:
            super(KeywordBase, self).__setattr__(attr, value)

    def __iadd__(self, child):
        """
        Support ' += ' inplace addition operator to append child keywords
        
        @param child: Keyword container, Iterator, or keyword string
        """
        self.append(child)
        return self

    def append(self, child):
        """
        Append a child keyword item or container
        
        @param child: Keyword container, Iterator, or keyword string 
        """

        if child is None:
            warn('Got None, expected a keyword object or container.')
            return

        if not self.format in (3, 5):
            raise ValueError('Keyword format % d does not support child keywords.' % self.format)

        # append strings and keyword containers in whole
        if isinstance(child, (KeywordBase, KeywordSet
                              , eventmonitor.ExpressionBlock
                              , eventmonitor.EventFunction
                              , eventmonitor.AlgebraExpression
                              , eventmonitor.LogicalExpression
                              , str)):
            self.children.append(child)

        # append iterators individually
        elif isinstance(child, (list, tuple, set)):
            for item in child:
                self.children.append(item)

        # TODO: handle dictionaries

        else:
            msg = 'Type % s can not be combined with Keyword' % (type(child),)
            raise TypeError(msg)

    def __repr__(self):
        """
        Represent a keyword as a dictionary of it's fields
        """
        d = {}

        d['fields'] = []
        for f in self.__fields__:
            d['fields'].append({'name':f
                                , 'value':getattr(self, f)
#                                , 'type':object.__getattribute__(self, f).width
                                })

        if self.add_vals:
            d['add_vals'] = []
            for attr in self.add_vals:
                d['add_vals'].append({'name':attr, 'value':getattr(self, attr)})
    #            d[attr] = getattr(self, attr)

        if self.children:
            d['children'] = []
            for child in self.children:
                d['children'].append(child.__repr__())

        if self.__supplemental__:
            d['supplemental'] = self.__supplemental__

        return str({self.__class__.__name__:d})

    def __str__(self):
        # #TODO: print children as well

        # NOTE: Using object.__getattribute__ to bypass the KeywordBase __getattribute__ method

        s = ''

#        if self.name:
#            s += '! %s\n' % self.name

#        print self.name

        if self.comment:
            comment = self.comment.strip()
            lines = comment.split('\n')

            if len(lines) > 1:
                s += 'COMMENT\n'

                for line in lines:
                    s += '  %s\n' % line

                s += 'END\n'

            else:
                s += '! %s\n' % comment

        #---Format 0
        if self.format == 0:
            # single line keyword format
            #  MNEMONIC  [field 1, ..., field n]

            s += '%-10s' % self.mnemonic
            for f in self.__fields__:
#                print '\t' + f
                # get the string representation of field objects
                s += str(object.__getattribute__(self, f))

            # append any add values to the string
            for attr in self.add_vals:
                s += '%10s' % str(getattr(self, attr))

        #---Format 1
        elif self.format == 1:
            # two line format
            #  MNEMONIC
            #  [field 1, ..., field n]
            s += '%-10s\n' % self.mnemonic

            for f in self.__fields__:
                s += str(object.__getattribute__(self, f))

            for f in self.add_vals:
                s += '%10s' % str(getattr(self, f))

        #---Format 2
        elif self.format == 2:
            # multi line format
            #  MNEMONIC
            #  [field 1
            #  ...
            #  field n]
            s += '%-10s\n' % self.mnemonic

            for f in self.__fields__:
                s += '%s\n' % str(object.__getattribute__(self, f))

            for f in self.add_vals:
                s += '%s\n' % str(getattr(self, f))

        #---Format 3
        elif self.format == 3:
            # Keyword Block format
            #  MNEMONIC  [[field 1, ..., field n]
            #  [child 1
            #  ...
            #  child n]]
            #  END

            s += '%-10s' % self.mnemonic

            # add field strings
            for f in self.__fields__:
                s += str(object.__getattribute__(self, f))

            for f in self.add_vals:
                s += '%10s' % str(getattr(self, f))

            s = s.rstrip()
            s += '\n'

            # add children
            for c in self.children:
                s += '%s\n' % (str(c).strip(),)

            s += 'END\n'
            s += '! End %s\n' % self.mnemonic

        #---Format 4
        elif self.format == 4:
            # PARMS format
            #
            s += '%-10s' % self.mnemonic

            # handle add-hoc keywords
            if self.add_vals:
                s += '%s' % str(getattr(self, self.add_vals[0]))
                vals = ', %s' % (', '.join([str(getattr(self, f)).strip()
                                  for f in self.add_vals[1:]]))
                s += '   PARMS(%s)\n' % vals

            else:
                s += '%s' % object.__getattribute__(self, self.__fields__[0])
                vals = ', '.join([str(object.__getattribute__(self, f)).strip()
                                  for f in self.__fields__[1:]])

                s += '   PARMS(%s)\n' % vals

            # PARMS can be split between two lines
            # #TODO: double check PARMS line continuation
            if len(s) > 80:
                s = '%s&\n%s' % (s[:80], s[80:])

        # #TODO: combine with format 3 and boolean "END" flag
        #---Format 5
        elif self.format == 5:
            # single line keyword format with supplemental/children
            #  MNEMONIC  [[fld 1, ..., fld n]
            #  [supplemental 1,
            #  ...,
            #  supplemental n]
            #  [child 1,
            #  ...,
            #  child n]]

            s += '%-10s' % self.mnemonic

            for f in self.__fields__:
                if not f in self.__supplemental__:
                    s += str(object.__getattribute__(self, f))

            for f in self.add_vals:
                s += '%10s' % str(getattr(self, f))

            s = s.rstrip()
            s += '\n'

            for f in self.__supplemental__:
                s += '%s\n' % str(object.__getattribute__(self, f))

            # add children
            for c in self.children:
                s += '%s\n' % c

#        #---Format 6
#        elif self.format == 6:
#            #multi line format
#            #  MNEMONIC
#            #  [field 1
#            #  ...
#            #  field n]
#            #  END
#            s += '%-10s\n' % self.mnemonic
#
#            for f in self.__fields__:
#                s += '%s\n' % str(object.__getattribute__(self, f))
#
# #            for f in self.add_vals:
# #                s += '%s\n' % str(getattr(self, f))
# #
#            s += 'END\n'

        else:
            raise KeyError('Keyword format not implemented: %s' % self.format)

        return s  # '%s\n' % s.rstrip()

class AddHocKeyword(KeywordBase):
    def __init__(self, mnemonic, field_vals=(), format=0, supplemental=()
                 , name='', comment='', **kargs):
        """
        A generic class KeywordBase class for keywords defined at run-time
        
        :param mnemonic:  FVS keyword identifier
        :param field_vals:  Iterator of field values
        :param format:  Keyword formatting code (see KeywordBase)
        :param name:  Descriptive identifier  
        :param comment:  Description of the keyword's purpose 
        """
        KeywordBase.__init__(self, mnemonic, name=name, comment=comment
                             , format=format, **kargs)
        self.mnemonic = mnemonic
        self.field_vals = field_vals

        fields = []
        fld_vals = {}
        for i, val in enumerate(field_vals):
            fn = 'fld_%d' % i
            fields.append(fn)
            fld = CharacterField(fn, ignore_case=True)
            setattr(self, fn, fld)
            fld_vals[fn] = val

        sup = []
        sup_vals = {}
        for i, val in enumerate(supplemental):
            fn = 'sup_%d' % i
            sup.append(fn)
            fields.append(fn)
            fld = CharacterField(fn, ignore_case=True)
            setattr(self, fn, fld)
            sup_vals[fn] = val

        self.__fields__ = tuple(fields)
        self.__supplemental__ = tuple(sup)

        for fld, val in fld_vals.items():
            setattr(self, fld, val)

        for sup, val in sup_vals.items():
            setattr(self, sup, val)

class KeywordSet(object):
    def __init__(self, rank=0, title=None, comment=None, top_level=False):
        """
        Represents a collection of FVS keywords.
        """

        self.rank = rank
        self.title = title
        self.comment = comment
        self.top_level = top_level
        self.parent = None

        if top_level and self.title is None:
            self.title = 'pyfvs'

        self.items = []

    def __iadd__(self, kwd):
        """
        Implement the '+=' operator to append a keyword object.
        """
        self.append(kwd)
        return self  # inplace operators are expected to return self

    def __add__(self, kwd):
        """
        Implement the '+' operator to return a copy of self with item appended.
        """
        c = copy.deepcopy(self)
        c.append(kwd)
        return c

    def append(self, kwd):
        """
        Append a keyword object.
        """
        if isinstance(kwd, KeywordSet):
            kwd.top_level = False
            kwd.parent = self

        self.items.append(kwd)

    def insert(self, idx, kwd):
        """
        Insert a keyword object at a specific index position.
        """
        self.items.insert(idx, kwd)

    def __repr__(self):
        """
        Return a dictionary representation of the keywords.
        """
        d = []
        for item in self.items:
            d.append(item.__repr__())

        return str({self.__class__.__name__:d})

    def __str__(self):
        """
        Return a FVS formatted keyword string.
        """
#         self.items.sort(cmp=self._sort)

        s = ''

        if self.title:
            s += '\n\n! Begin: %s\n' % self.title.strip()

        if self.comment:
            lines = self.comment.strip().split('\n')
            s += '!*******************\n'
            for line in lines:
                s += '!  {}\n'.format(line.strip())
            s += '!*******************\n'

#         for keyword in self.items:
#             s += '%s\n' % keyword.__str__()  # .strip()
        s += '\n'.join(str(i) for i in self.items)
        s += '\n'

        if self.title:
            s += '! End: %s\n' % self.title.strip()

        if self.top_level:
            # Close out the keyword file
            s += '\n' + str(PROCESS())
            s += '\n' + str(STOP())

        return s

    # #TODO: use the builtin operator.attrgetter to sort on rank
    def _sort(self, k1, k2):
        """
        Sort the keywords to ensure FVS reads them in the proper order
        
        #FIXME: Test and fully implement for implemented keywords 
        """
        try:
            if k1.rank < k2.rank: return -1
            if k1.rank > k2.rank: return 1

        except AttributeError:
            return 0

        except:
            raise

        return 0

    def __iter__(self):
        for item in self.items:
            yield item

    def find(self, kwd):
        """
        Search for instances of a keyword.
        """

        objs = []

        for item in self.items:
            try:
                if item.mnemonic == kwd:
                    objs.append(item)
                    continue
            except:
                pass

            if isinstance(item, KeywordSet):
                r = item.find(kwd)
                objs.extend(r)
                continue

        return objs

    def write(self, path):
        """
        Write the formatted keywords to a text file.
        """
        try:
            with open(path, 'w') as f:
                f.write(str(self))
        except:
            raise

# #--Keyword Template--
class KEYWORDNAME(KeywordBase):
    fld_1 = CharacterField('Field Name')
    def __init__(self, fld_1, **kargs):
        """
        Document the keyword

        @param fld_1:  Field description
        """
        # init the Keyword parent
        KeywordBase.__init__(self, 'TEMPLATE', 'Keyword Template', **kargs)

        self.fld_1 = fld_1

class DB_DATABASE(KeywordBase):
    def __init__(self, **kargs):
        """
        Define a set of database keywords
        """
        # init the Keyword parent
        KeywordBase.__init__(self, 'DATABASE', 'Database keyword set'
                , format=3, children=[], **kargs
                )

class DB_DSNIN(KeywordBase):
    dsn = TextField('Input DSN')

    def __init__(self, dsn, **kargs):
        """
        Input database DSN

        @param dsn:  ODBC DSN;uid;pwd or file name (.mdb,.xls) 
        """
        KeywordBase.__init__(self, 'DSNIN'
                             , 'Input database DSN', format=1, **kargs)

        self.dsn = dsn

class DB_STANDSQL(KeywordBase):
    sql = TextField('Stand Init SQL')
    end = TextField('End Tag')

    def __init__(self, sql, **kargs):
        """
        """
        KeywordBase.__init__(self, 'STANDSQL'
                             , 'Stand Init SQL', format=2, **kargs)

        self.sql = sql
        self.end = 'EndSQL'

class DB_TREESQL(KeywordBase):
    sql = TextField('Tree Init SQL')
    end = TextField('End Tag')

    def __init__(self, sql, **kargs):
        """
        """
        KeywordBase.__init__(self, 'TREESQL'
                             , 'Tree Init SQL', format=2, **kargs)

        self.sql = sql
        self.end = 'EndSQL'

class DB_DSNOUT(KeywordBase):
    dsn = TextField('Output DSN')

    def __init__(self, dsn, **kargs):
        """
        Output database DSN

        @param dsn:  ODBC DSN or file name (.mdb,.xls) 
        """
        KeywordBase.__init__(self, 'DSNOUT'
                             , 'Output database DSN', format=1, **kargs)

        self.dsn = dsn

class DB_SUMMARY(KeywordBase):
    def __init__(self, **kargs):
        """
        Send the summary table to the output database
        """
        KeywordBase.__init__(self, 'SUMMARY'
                             , 'Send stand summary to database', **kargs)

class DB_TREELIST(KeywordBase):
    output_dest = IntegerField('Output Destination Code')
    spp_fmt = IntegerField('Species Format')

    def __init__(self, output_dest=1, spp_fmt=0, **kargs):
        """
        Output the treelist to the database

        @param output_dest:
                1: send to DB and file
                2: send treelist to DB only
        
        @param spp_fmt:  Species code format
                    0: format of last input tree record
                    1: FVS alpha code format
                    2: FIA code format
                    3: USDA plants symbol format
        """
        KeywordBase.__init__(self, 'TREELIST'
                         , 'Send treelist to database', **kargs)

        self.output_dest = output_dest
        self.spp_fmt = spp_fmt

class DB_CUTLIST(KeywordBase):
    output_dest = IntegerField('Output Destination Code')
    spp_fmt = IntegerField('Species Format')

    def __init__(self, output_dest=1, spp_fmt=0, **kargs):
        """
        Output the cutlist to the database

        @param output_dest: 2: send cutlist to DB only; 1: send to DB and file
        @param spp_fmt:  Species code format
                    0: format of last input tree record
                    1: FVS alpha code format
                    2: FIA code format
                    3: USDA plants symbol format
        """

        KeywordBase.__init__(self, 'CUTLIST'
                             , 'Send cutlist to database', **kargs)

        self.output_dest = output_dest
        self.spp_fmt = spp_fmt

class DB_ATRTLIST(KeywordBase):
    output_dest = IntegerField('Output Destination Code')
    spp_fmt = IntegerField('Species Format')

    def __init__(self, output_dest=1, spp_fmt=0, **kargs):
        """
        Output the after treament treelist to the database

        @param output_dest: - 2: DB only; 1: send to DB and file
        @param spp_fmt: - Species code format
                    0: format of last input tree record
                    1: FVS alpha code format
                    2: FIA code format
                    3: USDA plants symbol format
        """
        KeywordBase.__init__(self, 'ATRTLIST'
                             , 'Send after treament treelist to database'
                             , **kargs)

        self.output_dest = output_dest
        self.spp_fmt = spp_fmt

class DB_COMPUTE(KeywordBase):
    append_new = IntegerField('Append New')
    output_pvt = IntegerField('Ignore Private')

    def __init__(self, append_new=0, output_pvt=0, **kargs):
        """
        Output user defined COMPUTE event monitor variables

        @param append_new:
        @param output_pvt:
        """
        KeywordBase.__init__(self, 'COMPUTE'
                         , 'Output user defined variables', **kargs)

        self.append_new = not append_new  # 0 has the effect of adding fields, the opposite of True/False in Python
        self.output_pvt = output_pvt

class DB_FUELSOUT(KeywordBase):
    output_dest = IntegerField('Output Destination Code')

    def __init__(self, output_dest=0, **kargs):
        """
        Write the fuels table to a database

        @param output_dest:
                    2: send the table to DB only
                    1: send to DB and file
        """
        KeywordBase.__init__(self, 'FUELSOUT'
             , 'Send the fuels report to a database', **kargs)

        self.output_dest = output_dest

class DB_DWDCVOUT(KeywordBase):
    output_dest = IntegerField('Output Destination Code')

    def __init__(self, output_dest=0, **kargs):
        """
        Output the down wood cover report to a database table

        @param output_dest:  Output Destination Code
                2: send the table to DB only
                1: send to the DB and file
        """
        KeywordBase.__init__(self
                , 'DWDCVOUT'
                , 'Write the downwood cover table to database'
                , **kargs)

        self.output_dest = output_dest

class DB_DWDVLOUT(KeywordBase):
    output_dest = IntegerField('Output Destination Code')

    def __init__(self, output_dest=0, **kargs):
        """
        Output the down wood volume report to a database table

        @param output_dest:  Output Destination Code
                2: send the table to DB only
                1: send to the DB and file
        """
        KeywordBase.__init__(self
                , 'DWDVLOUT'
                , 'Write the downwood volume table to database'
                , **kargs)

        self.output_dest = output_dest

class DB_STRCLASS(KeywordBase):
    output_dest = IntegerField('Output Destination Code')

    def __init__(self, output_dest=0, **kargs):
        """
        Output the stucture statistics report to a database table

        @param output_dest:  Output Destination Code
                2: send the table to DB only
                1: send to the DB and file
        """
        KeywordBase.__init__(self
                , 'STRCLASS'
                , 'Write the structure statistics table to database'
                , **kargs)

        self.output_dest = output_dest

class DB_SNAGSUM(KeywordBase):
    output_dest = IntegerField('Output Destination Code')

    def __init__(self, output_dest=0, **kargs):
        """
        Output the snag summary table to the database

        @param output_dest:  Output Destination Code
                2: send snag summary table to DB only
                1: send to DB and file
        """
        KeywordBase.__init__(self, 'SNAGSUM'
                         , 'Send snag summary table to database', **kargs)

        self.output_dest = output_dest

class DB_SNAGOUT(KeywordBase):
    output_dest = IntegerField('Output Destination Code')
    spp_fmt = IntegerField('Species Format')

    def __init__(self, output_dest=0, spp_fmt=0, **kargs):
        """
        Output the snag detail table to the database

        @param output_dest:  Output Destination Code
                2 - send snag detail table to DB only
                1 - send to DB and file
        
        @param spp_fmt:  Species code format
                    0 - format of last input tree record
                    1 - FVS alpha code format
                    2 - FIA code format
                    3 - USDA plants symbol format

        """
        KeywordBase.__init__(self, 'SNAGOUT'
                             , 'Send the snag detail table to database'
                             , **kargs)

        self.output_dest = output_dest
        self.spp_fmt = spp_fmt

class DB_CARBRPTS(KeywordBase):
    output_dest = IntegerField('Output Destination')

    def __init__(self, output_dest=0, **kargs):
        """
        Output the carbon reports to the database

        @param output_dest:  Output Destination Code 
                            2 - send carbon reports to DB only
                            1 - send to DB and file
        """
        KeywordBase.__init__(self, 'CARBRPTS'
                             , 'Send carbon reports to database', **kargs)

        self.output_dest = output_dest

class RESETAGE(KeywordBase):
    cycle = IntegerField('Cycle')
    age = IntegerField('Age')

    def __init__(self, cycle=0, age=0, **kargs):
        """
        Reset the age of the stand following a stand replacement disturbance

        @param cycle:  Cycle or year the new age is to initiate
        @param age:  New age of the stand
        """
        KeywordBase.__init__(self, 'RESETAGE', 'Reset Stand Age', **kargs)

        self.cycle = cycle
        self.age = age

class ESTAB(KeywordBase):
    cycle = IntegerField('Cycle')

    def __init__(self, cycle=0, **kargs):
        """
        Initiate a regeneration establishment keyword block

        @param cycle:  Cycle/year of the disturbance or beginning of regeneration
        """
        KeywordBase.__init__(self, 'ESTAB', 'Regeneration establishment'
                         , format=3, **kargs)

        self.cycle = cycle

class PLANT(KeywordBase):
    cycle = IntegerField('Cycle')
    species = CharacterField('Species')
    tpa = IntegerField('TPA')
    survival = IntegerField('Survival')
    age = IntegerField('Age')
    height = DecimalField('Height', precision=4)
    shade = IntegerField('Shade Code')

    def __init__(self, cycle, species, tpa=0, survival=100
                 , age=2, height=0, shade=0, **kargs
                 ):
        """
        Add a species planting record to an ESTAB keyword block

        @param cycle:  Cycle/Year of the planting (assumed to occur in spring)
                            Must be greater or equal to the associated ESTAB cycle.
        @param species:  Species code
        @param tpa:  Trees Per Acre of species planted
        @param survival:  Percent survival of this planting record 0-100
        @param age:  Average age of planted trees at plant year
        @param height:  Average height of planted trees five years after year
                            planted or at end of the cycle
        @param shade:  Shade code
                    0 - Seedlings occurance is uniform throughout the stand
                    1 - Occurance is more frequent with overstory
                    2 - Occurance is less frequent with overstory 
        """
        KeywordBase.__init__(self, 'PLANT', 'Planted Seedlings', **kargs)

        self.cycle = cycle
        self.species = species
        self.tpa = tpa
        self.survival = survival
        self.age = age
        self.height = height
        self.shade = shade

class NATURAL(PLANT):
    cycle = IntegerField('Cycle')
    species = CharacterField('Species')
    tpa = IntegerField('TPA')
    survival = IntegerField('Survival')
    age = IntegerField('Age')
    height = DecimalField('Height', precision=4)
    shade = IntegerField('Shade Code')
    # #TODO: shade is an EnumField

    def __init__(self, cycle, species, tpa=0, survival=100
                 , age=2, height=2, shade=0, **kargs):

        """
        Add a species natural regen record to an ESTAB keyword block

        Args
        ----
        *refer to PLANT keyword
        """
        KeywordBase.__init__(self, 'NATURAL'
                             , 'Natural Regeneration', **kargs)

        self.cycle = cycle
        self.species = species
        self.tpa = tpa
        self.survival = survival
        self.age = age
        self.height = height
        self.shade = shade

class TALLY(KeywordBase):
    cycle = IntegerField('Cycle Year')
    disturbance = IntegerField('Disturbance Year')

    def __init__(self, cycle=0, disturbance=0, **kargs):
        """
        Schedule a regeneration tally and disturbance
        
        cycle - Year/Cycle to schedule the tally
        disturbance - Year/Cycle of the disturbance
        """
        KeywordBase.__init__(self, 'TALLY', 'Tally', format=4, **kargs)

        self.cycle = cycle
        self.disturbance = disturbance

class TREEFMT(KeywordBase):
    format_1 = CharacterField('Format 1', default=''
                              , width=80, align_right=False)
    format_2 = CharacterField('Format 2', default=''
                              , width=80, align_right=False)

    __supplemental__ = (format_1, format_2)

    def __init__(self
            , fmt='(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,' \
                        '3(I2,I2),2I1,I2,2I3,2I1,F3.0)'
            , **kargs):
        """
        A fortran data format descriptor for tree data

        @param fmt:  Fortran formatting descriptor
        """
        KeywordBase.__init__(self, 'TREEFMT', 'Tree Data Format'
                             , format=2, **kargs)

        self.fmt = fmt

        # Ensure the format is enclosed in parentheses
        if self.fmt[0] != '(':
            self.fmt = '(' + self.fmt

        if self.fmt[0] != ')':
            self.fmt = self.fmt + ')'

        if len(self.fmt) <= 80:
            self.format_1 = self.fmt
        else:
            i = fmt.find(',', 75)
            self.format_1 = self.fmt[0:i]
            self.format_2 = self.fmt[i:]

class CYCLEAT(KeywordBase):
    year = IntegerField('Year')
    def __init__(self, year, **kargs):
        """
        Forces a cycle break at <year>.  This is used to produce to trigger
        a call to the event monitor.

        @param year:  Year within the simulation to add a cycle break
        """
        KeywordBase.__init__(self, 'CYCLEAT', 'Cycle Break year', **kargs)

        self.year = year

class TIMEINT(KeywordBase):
    cycle = IntegerField('Cycle')
    length = IntegerField('Length')

    def __init__(self, cycle=0, length=10, **kargs):
        """
        Time Interval, modifies the length of a given cycle

        @param cycle:  Cycle whose length is to be modified, 0 to modify all
        @param length:  New length of the cycle
        """
        KeywordBase.__init__(self, 'TIMEINT', 'Time Interval', **kargs)

        self.cycle = cycle
        self.length = length

class PROCESS(KeywordBase):
    def __init__(self):
        """
        Prompts FVS to execute the projection based on the keywords.
        """
        KeywordBase.__init__(self, 'PROCESS', 'Process Keywords')

class STOP(KeywordBase):
    def __init__(self):
        """
        Prompts FVS to terminate the current projection
        """
        KeywordBase.__init__(self, 'STOP', 'Stop processing and exit FVS')

class NUMCYCLE(KeywordBase):
    cycles = IntegerField('Cycles')
    def __init__(self, cycles=1, **kargs):
        """
        Number of simulation cycles to run

        @param cycles:  Number of cycles to run in a simulation
        """
        KeywordBase.__init__(self, 'NUMCYCLE', 'Number of Cycles', **kargs)

        self.cycles = cycles

class TREELIST(KeywordBase):
    first_cycle = IntegerField('First Report Cycle')
    file_num = IntegerField('File Number')
    header_style = IntegerField('Header Style')
    cycle_zero = IntegerField('Cycle Field')
    live_dead = BooleanField('Live and Dead Data')
    foo_1 = NullField('foo_1')
    dbh_increment = IntegerField('Diameter Increment Estimates')

    def __init__(self, first_cycle=0, file_num=3, header_style=1
                 , cycle_zero=0, live_dead=0, dbh_increment=0
                 , **kargs):
        """
        Produce the treelist file

        @param firstCycle:  First simulation cycle to report
        @param fileNum:  Fortran file reference number
        @param headerStyle:  Tree data column header style
                                1=encoded;0=human;-1=None
        @param cycleZero:  Control output of cycle zero tree data
                                0=all; 1=one only; 2=zero only
        @param liveDead:  Request dead tree record report, not just current cycle mortality
        
        @param dbhIncrement:  Include diamter growth estimates
        """
        self.first_cycle = first_cycle
        self.file_num = file_num
        self.header_style = header_style
        self.cycle_zero = cycle_zero
        self.live_dead = live_dead
        self.dbh_increment = dbh_increment

        KeywordBase.__init__(self, 'TREELIST', 'Write the Tree List Data File', **kargs)

class TREEDATA(KeywordBase):
    file_num = IntegerField('File Number')
    plot_site = IntegerField('Plot Site Descriptors')
    min_dbh = DecimalField('Minimum DBH')
    max_dbh = DecimalField('Maximum DBH')
    spp_exclude = CharacterField('Exclude Species')

    def __init__(self
            , file_num=2, plot_site=None
            , min_dbh=0.0, max_dbh=999.9
            , spp_exclude='', **kargs):
        """
        Read and Filter Tree Data
        
        @param file_num: File unit number
        @param plot_site: Read plot specific site descriptors
        @param min_dbh: Minimum tree DBH to retain
        @param max_dbh:: Maximum tree DBH to retain
        @param spp_exclude: Species to exclude
        """
        self.file_num = file_num
        self.plot_site = plot_site
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh
        self.spp_exclude = spp_exclude

        KeywordBase.__init__(self, 'TREEDATA', 'Read Tree Records', **kargs)

class CUTLIST(KeywordBase):
    first_cycle = IntegerField('First Report Cycle')
    file_num = IntegerField('File Number')
    header_style = IntegerField('Header Style')
    foo_1 = NullField()
    foo_2 = NullField()
    rpt_format = IntegerField('Report Format')

    def __init__(self, first_cycle=0, file_num=3, header_style=1
            , rpt_format=0, **kargs):
        """
        Produce out the cut tree list

        @param first_cycle:  First cycle to report; 0 to report all
        @param file_num:  Fortran file reference number
        @param header_style:  Tree data column header style, 1=encoded;0=human;-1=None
        @param rpt_format:  Cut list data format 0=Current; 1=Old
        """
        KeywordBase.__init__(self, 'CUTLIST'
                             , 'Write the Cut Tree List Data File'
                             , **kargs)

        self.first_cycle = first_cycle
        self.file_num = file_num
        self.header_style = header_style
        self.rpt_format = rpt_format

class ATRTLIST(KeywordBase):
    first_cycle = IntegerField('First Reported Cycle')
    file_num = IntegerField('File Number')
    header_style = IntegerField('Header Style')

    def __init__(self, first_cycle=0, file_num=3, header_style=1, **kargs):
        """
        Produce the after treament tree list

        @param firstCycle:  First cycle to report
                                0 to report all
        @param fileNum:  FVS file number to write to
        @param headerStyle:  Tree data column header style
                                1=encoded;0=human;-1=None
        """
        KeywordBase.__init__(self, 'ATRTLIST'
                             , 'Write the after treament tree data file'
                             , format=0, **kargs)

        self.first_cycle = first_cycle
        self.file_num = file_num
        self.header_style = header_style

class FFE_FMIN(KeywordBase):
    def __init__(self, **kargs):
        """
        Fire and Fuels extension keyword block
        """
        KeywordBase.__init__(self, 'FMIN', 'Initialize FFE'
                , format=3, children=[], **kargs)

class FFE_CARBCALC(KeywordBase):
    method = IntegerField('Accounting Method')
    units = IntegerField('Accounting Units')
    decay_rate = DecimalField('Decay Rate', precision=5)
    softwood_dbh = DecimalField('Softwood DBH Break', precision=5)
    hardwood_dbh = DecimalField('Hardwood DBH Break', precision=5)

    def __init__(self, method=0, units=0, decay_rate=0.0425
                 , softwood_dbh=9.0, hardwood_dbh=11.0
                 , **kargs):
        """
        Set the carbon accounting parameters

        @param method:  Carbon accounting method
                            0 - FFE algorithms
                            1 - Jenkins algorithm
        @param units:  Carbon accounting units
                            0 - US tons per acre
                            1 - metric tonnes per hectare                             ??? Units
                            2 - metric tonnes per acre
        @param decay_rate:  Annual decay rate for below ground dead wood
        @param softwood_dbh:  DBH breakpoint for softwood sawlogs in product pool
        @param hardwood_dbh:  DBH breakpoint for hardwood sawlogs in product pool
        """
        KeywordBase.__init__(self, 'CARBCALC', 'Carbon Accounting Parameters'
                         , format=0, **kargs)

        self.method = method
        self.units = units
        self.decay_rate = decay_rate
        self.softwood_dbh = softwood_dbh
        self.hardwood_dbh = hardwood_dbh

class FFE_CARBCUT(KeywordBase):
    first_cycle = IntegerField('First report cycle')
    rpt_years = IntegerField('Report years')
    rpt_interval = IntegerField('Report interval')

    def __init__(self, first_cycle=1, rpt_years=200, rpt_interval=1, **kargs):
        """
        Produce the FFE harvested products carbon report

        @param first_cycle:  First cycle to report; 0 to report all
        @param rpt_years:  Number of years to output
        @param rpt_interval:  Output interval
        """
        KeywordBase.__init__(self, 'CARBCUT', 'Harvested products report'
                             , format=0, **kargs)

        self.first_cycle = first_cycle
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval

class FFE_CARBREPT(KeywordBase):
    first_cycle = IntegerField('First report cycle')
    rpt_years = IntegerField('Report years')
    rpt_interval = IntegerField('Report interval')

    def __init__(self, first_cycle=1, rpt_years=200, rpt_interval=1, **kargs):
        """
        Produce the FFE stand carbon report

        @param first_cycle:  First cycle to report; 0 to report all
        @param rpt_years:  Number of years to output
        @param rpt_interval:  Output interval
        """
        KeywordBase.__init__(self, 'CARBREPT', 'Stand Carbon Report'
                             , format=0, **kargs)

        self.first_cycle = first_cycle
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval

class FFE_DWDCVOUT(KeywordBase):
    first_cycle = IntegerField('First report cycle')
    rpt_years = IntegerField('Report years')
    rpt_interval = IntegerField('Report interval')

    def __init__(self, first_cycle=1, rpt_years=200, rpt_interval=1, **kargs):
        """
        Produce the FFE down woody debris cover report

        @param first_cycle:  First cycle to report; 0 to report all
        @param rpt_years:  Number of years to output
        @param rpt_interval:  Output interval
        """
        KeywordBase.__init__(self, 'DWDCVOUT', 'Down Woody Debris Cover Report'
                             , format=0, **kargs)

        self.first_cycle = first_cycle
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval

class FFE_DWDVLOUT(KeywordBase):
    first_cycle = IntegerField('First report cycle')
    rpt_years = IntegerField('Report years')
    rpt_interval = IntegerField('Report interval')

    def __init__(self, first_cycle=1, rpt_years=200, rpt_interval=1, **kargs):
        """
        Produce the FFE down woody debris volume report

        @param first_cycle:  First cycle to report; 0 to report all
        @param rpt_years:  Number of years to output
        @param rpt_interval:  Output interval
        """
        KeywordBase.__init__(self, 'DWDVLOUT', 'Down Woody Debris Volume Report'
                             , format=0, **kargs)

        self.first_cycle = first_cycle
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval

class FFE_FUELOUT(KeywordBase):
    first_cycle = IntegerField('First report cycle')
    rpt_years = IntegerField('Report years')
    rpt_interval = IntegerField('Report interval')

    def __init__(self, first_cycle=1, rpt_years=200, rpt_interval=1, **kargs):
        """
        Produce the FFE detailed fuels report

        @param first_cycle:  First cycle to report; 0 to report all
        @param rpt_years:  Number of years to output
        @param rpt_interval:  Output interval
        """
        KeywordBase.__init__(self, 'FUELOUT', 'Detailed Fuel Report'
                         , format=0, **kargs)

        self.first_cycle = first_cycle
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval

class FFE_FUELINIT(KeywordBase):
    fuel_0_1 = DecimalField('Fuel 0-1', precision=5)
    fuel_1_3 = DecimalField('Fuel 1-3', precision=5)
    fuel_3_6 = DecimalField('Fuel 3-6', precision=5)
    fuel_6_12 = DecimalField('Fuel 6-12', precision=5)
    fuel_12_20 = DecimalField('Fuel 12-20', precision=5)
    fuel_litter = DecimalField('Fuel litter', precision=5)
    fuel_duff = DecimalField('Fuel duff', precision=5)
    fuel_0_25 = DecimalField('Fuel 0.0-0.25', precision=5)
    fuel_25_1 = DecimalField('Fuel 0.25-1.0', precision=5)
    fuel_20_35 = DecimalField('Fuel 20-35', precision=5)
    fuel_35_50 = DecimalField('Fuel 35-50', precision=5)
    fuel_gt_50 = DecimalField('Fuel >= 50', precision=5)

    def __init__(self, fuel_0_1=-1.0, fuel_1_3=-1.0
            , fuel_3_6=-1.0, fuel_6_12=-1.0, fuel_12_20=-1.0
            , fuel_litter=-1.0, fuel_duff=-1.0
            , fuel_0_25=-1.0, fuel_25_1=-1.0, fuel_20_35=-1.0
            , fuel_35_50=-1.0, fuel_gt_50=-1.0
            , **kargs):
        """
        Set the initial hard (sound) dead fuel loadings (tons/acre)
        
        @param fuel_0_1:  Hard Fuel 0-1 inches
        @param fuel_1_3:  Hard Fuel 1-3 inches
        @param fuel_3_6:  Hard Fuel 3-6 inches
        @param fuel_6_12:  Hard Fuel 6-12 inches
        @param fuel_12_20:  Hard Fuel 12-20 inches 
        @param fuel_litter:  Hard Fuel litter
        @param fuel_duff:  Hard Fuel duff
        @param fuel_0_25:  Hard Fuel 0.0-0.25 inches
        @param fuel_25_1:  Hard Fuel 0.25-1.0 inches
        @param fuel_20_35:  Hard Fuel 20-35 inches
        @param fuel_35_50:  Hard Fuel 35-50 inches 
        @param fuel_gt_50:  Hard Fuel >= 50 inches
        """
        KeywordBase.__init__(self, 'FUELINIT', 'Hard Dead Fuel Loadings'
                         , format=0, **kargs)

        self.fuel_0_1 = fuel_0_1
        self.fuel_1_3 = fuel_1_3
        self.fuel_3_6 = fuel_3_6
        self.fuel_6_12 = fuel_6_12
        self.fuel_12_20 = fuel_12_20
        self.fuel_litter = fuel_litter
        self.fuel_duff = fuel_duff
        self.fuel_0_25 = fuel_0_25
        self.fuel_25_1 = fuel_25_1
        self.fuel_20_35 = fuel_20_35
        self.fuel_35_50 = fuel_35_50
        self.fuel_gt_50 = fuel_gt_50

class FFE_FUELFOTO(KeywordBase):
    series_ref = IntegerField('Photo Series')
    photo_num = IntegerField('Photo Code')

    def __init__(self, series_ref, photo_num, **kargs):
        """
        
        @param series_ref: Photo series reference number
        @param photo_num: Photo refernce code
        """
        KeywordBase.__init__(self, 'FUELFOTO', 'Surface Fuel Loading reference photo'
                             , format=0, **kargs)

        self.series_ref = series_ref
        self.photo_num = photo_num

class FFE_FUELSOFT(KeywordBase):
    fuel_0_25 = DecimalField('Fuel 0-25', precision=5)
    fuel_25_1 = DecimalField('Fuel 0.25-1.0', precision=5)
    fuel_1_3 = DecimalField('Fuel 1-3', precision=5)
    fuel_3_6 = DecimalField('Fuel 3-6', precision=5)
    fuel_6_12 = DecimalField('Fuel 6-12', precision=5)
    fuel_12_20 = DecimalField('Fuel 12-20', precision=5)
    fuel_20_35 = DecimalField('Fuel 20-35', precision=5)
    fuel_35_50 = DecimalField('Fuel 35-50', precision=5)
    fuel_gt_50 = DecimalField('Fuel >= 50', precision=5)

    def __init__(self
            , fuel_0_25=0.0, fuel_25_1=0.0
            , fuel_1_3=0.0, fuel_3_6=0.0
            , fuel_6_12=0.0, fuel_12_20=0.0
            , fuel_20_35=0.0, fuel_35_50=0.0
            , fuel_gt_50=0.0, **kargs):
        """
        Set the initial soft (rotten) dead fuel loadings (tons/acre)
        
        @param fuel_0_25:  Soft Fuel 0.0-0.25 inches
        @param fuel_25_1:  Soft Fuel 0.25-1.0 inches
        @param fuel_1_3:  Soft Fuel 1-3 inches
        @param fuel_3_6:  Soft Fuel 3-6 inches
        @param fuel_6_12:  Soft Fuel 6-12 inches
        @param fuel_12_20:  Soft Fuel 12-20 inches
        @param fuel_20_35:  Soft Fuel 20-35 inches
        @param fuel_35_50:  Soft Fuel 35-50 inches
        @param fuel_gt_50:  Soft Fuel >= 50 inches
        """
        KeywordBase.__init__(self, 'FUELSOFT', 'Soft Dead Fuel Loadings'
                             , format=0, **kargs)

        self.fuel_0_25 = fuel_0_25
        self.fuel_25_1 = fuel_25_1
        self.fuel_1_3 = fuel_1_3
        self.fuel_3_6 = fuel_3_6
        self.fuel_6_12 = fuel_6_12
        self.fuel_12_20 = fuel_12_20
        self.fuel_20_35 = fuel_20_35
        self.fuel_35_50 = fuel_35_50
        self.fuel_gt_50 = fuel_gt_50

class FFE_SNAGOUT(KeywordBase):
    first_cycle = IntegerField('First Report Cycle')
    rpt_years = IntegerField('Report Years')
    rpt_interval = IntegerField('Report Interval')

    def __init__(self, first_cycle=1, rpt_years=200, rpt_interval=1, **kargs):
        """
        Produce the FFE snag detail report

        @param first_cycle:  First cycle to report; 0 to report all
        @param rpt_years:  Number of years to output
        @param rpt_interval:  Report interval
        """

        KeywordBase.__init__(self, 'SNAGOUT', 'Snag Detail Report'
                             , format=0, **kargs)

        self.first_cycle = first_cycle
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval

class FFE_SNAGSUM(KeywordBase):
    suppress = BooleanField('Suppress')

    def __init__(self, suppress=0, **kargs):
        """
        Produce the FFE snag summary report
        
        @param suppress:  If not zero, output is suppressed
        """
        KeywordBase.__init__(self, 'SNAGSUM', 'Snag Summary Report'
                             , format=0, **kargs)

        self.suppress = suppress

class MGMTID(KeywordBase):
    mgmt_id = CharacterField('Management ID', width=4, align_right=False)
    def __init__(self, mgmt_id='NONE', **kargs):
        """
        Management Regime Id

        @param mgmtId:  Management regime identification string
        """
        KeywordBase.__init__(self, 'MGMTID', 'Management Regime ID'
                         , format=1, **kargs)

        self.mgmt_id = mgmt_id

class MANAGED(KeywordBase):
    year = IntegerField('Year')
    managed = BooleanField('Managed Flag')
    def __init__(self, year=0, managed=True, **kargs):
        """
        Indicates the stand is managed (eg. a plantation).
        
        @param year:  Year or cycle after which the stand is considered managed
        @param managed:  Flag indicating the stand is managed 
        """
        KeywordBase.__init__(self, 'MANAGED', 'Managed Stand Flag', **kargs)

        self.year = year
        self.managed = managed

class SETSITE(KeywordBase):
    cycle = IntegerField('Cycle')
    hab_type = IntegerField('Hab. Type')
    ba_max = IntegerField('BAA Max')
    species = CharacterField('Species')
    site_idx = IntegerField('Site Index')
    pct_flag = BooleanField('Percent Flag')
    sdi_max = IntegerField('SDI Max')

    def __init__(self, cycle=1, hab_type=None, ba_max=None
            , species='ALL', site_idx=None, pct_flag=None, sdi_max=None
            , **kargs
            ):
        """
        Adjust the site variables for a stand

        Args
        ----
        @param cycle:  First year or cycle the adjustment applies to
        @param hab_type:  Habitat type, potential vegetation FVS sequence number
        @param ba_max:  New maximum basal area
        @param species: Species code the adjustment applies to
        @param site_idx:  New site index value or percent change
        @param pct_flg:  1 if site_idx is a percentage change
        @param sdi_max:  New maximum stand density index
        """
        KeywordBase.__init__(self, 'SETSITE', 'Adjust Site Variables', **kargs)

        self.cycle = cycle
        self.hab_type = hab_type
        self.ba_max = ba_max
        self.species = species
        self.site_idx = site_idx
        self.pct_flag = pct_flag
        self.sdi_max = sdi_max

class SITECODE(KeywordBase):
    species = CharacterField('Site Species')
    site_index = IntegerField('Site Index')
    spp_ovr = BooleanField('Site Species Overide')
    def __init__(self, species, site_index, spp_ovr=None, **kargs):
        """
        Site index species code

        @param site_species:  Species code
        @param site_index:  Site species site index value
        @param spp_ovr: Site species override flag
        """
        KeywordBase.__init__(self, 'SITECODE', 'Site Index Species Code'
                             , format=0, **kargs)

        self.species = species
        self.site_index = site_index
        self.spp_ovr = spp_ovr

class STDIDENT(KeywordBase):
    stand_id = CharacterField('Stand ID', width=26
                              , align_right=False, ignore_case=True)
    description = CharacterField('Description', width=54
                                 , align_right=False, ignore_case=True)

    def __init__(self, stand_id='', description='', **kargs):
        """
        Stand Identification

        Args
        ----
        @param id:  Stand identification.  Defaults to an integer
                        representing the current local time in seconds
        @param description:  Stand description
        """
        KeywordBase.__init__(self, 'STDIDENT', 'Stand Identification'
                             , format=1, **kargs)

        if not stand_id:
            stand_id = datetime.datetime.now().strftime('%Y%m%d_%H%M%S')

        self.stand_id = stand_id
        self.description = description

class STANDCN(KeywordBase):
    stand_cn = CharacterField('Stand Control Number', width=40
                              , align_right=False, ignore_case=True)

    def __init__(self, stand_cn=0, **kargs):
        """
        Stand Control Number - Veg database primary key

        @param stand_cn: Stand control number, a primary key into the 
                            vegetation database
        """
        KeywordBase.__init__(self, 'STANDCN', 'Stand Control Number'
                             , format=1, **kargs)

        self.stand_cn = stand_cn

class STDINFO(KeywordBase):
    location = IntegerField('Location')
    pot_veg_code = CharacterField('Potential Veg.', upper_case=True)
    age = IntegerField('Age')
    aspect = IntegerField('Aspect')
    slope = IntegerField('Slope Code')
    elev_code = IntegerField('Elevation Code')
    pot_veg_ref = IntegerField('Potential Veg. Reference')

    def __init__(self
            , location=0
            , pot_veg_code=''
            , age=0
            , aspect=0
            , slope=0
            , elev_code=0
            , pot_veg_ref=None
            , **kargs
            ):
        """
        Stand Information Keyword
        
        ***Note: PV reference code is field 7 even though the documentation says latitude 

        @param location:  FVS variant specific administrative location
        @param pot_veg_code:  Potential Vegetation code
        @param age:  Stand age in years
        @param aspect:  0-360 degrees
        @param slope:  Slope in percent
        @param elevCode:  Elevation as hundreds of feet
        @param pvRefCode:  Potential vegetation translation code
        """

        KeywordBase.__init__(self, 'STDINFO', 'Stand Information'
                             , format=0, **kargs)

        self.location = location
        self.pot_veg_code = pot_veg_code
        self.age = age
        self.aspect = aspect
        self.slope = slope
        self.elev_code = elev_code
        self.pot_veg_ref = pot_veg_ref

        if self.elev_code > 99:
            info('Elevation code {} is greater than 99, assuming feet'.format(self.elev_code))
            self.elev_code = self.elev_code / 100.

class INVYEAR(KeywordBase):
    year = IntegerField('year')
    def __init__(self, year=None, **kargs):
        """
        Stand Inventory Year

        @param year:  Calendar year the inventory data represents
        """
        KeywordBase.__init__(self, 'INVYEAR', 'Inventory Year'
                             , format=0, **kargs)

        # default to current calendary year
        # #TODO: should this be changed to current growth year???
        if year == None:
            year = datetime.datetime.now().year

        self.year = year

class xxxCARBREPT(KeywordBase):
    init_year = IntegerField('Initial Report Year')
    rpt_years = IntegerField('Report Duration')
    rpt_interval = IntegerField('Report Interval')

    def __init__(self, init_year=0, rpt_years=100, rpt_interval=10, **kargs):
        """
        Initialize the FFE Carbon reports

        @param init_year:  Initial year of the report cycle
        @param rptYears:  Report duration, number of years after initYear to report
        @param rptInterval:  Report interval, years between reports
        """
        KeywordBase.__init__(self, 'CARBREPT', 'Carbon Reports', format=0, **kargs)

        self.init_year = init_year
        self.rpt_years = rpt_years
        self.rpt_interval = rpt_interval
        self.kargs = kargs

class DESIGN(KeywordBase):
    large_tree_baf = DecimalField('Large Tree BAF', precision=5)
    small_tree_exp = DecimalField('Small Tree Expansion', precision=1)
    break_dbh = DecimalField('Large Tree Break DBH', precision=1)
    sample_size = IntegerField('Sample Point Count')
    not_stockable = IntegerField('Not Stockable Plot Count')
    sample_weight = IntegerField('Sample Weight')
    stockable_prop = DecimalField('Stockable Proportion', precision=3)

    def __init__(self, large_tree_baf=None, small_tree_exp=1
            , break_dbh=999.9, sample_size=None, not_stockable=None
            , sample_weight=None, stockable_prop=None
            , **kargs):
        """
        Define a sampling design

        Defaults to pre-expanded trees-per-acre tree list

        @param large_tree_baf:  A positive value is interpreted as a basal area factor
                                  for horizontal angle gauge (prism). A negative value
                                  is interpreted as the inverse of a large-tree fixed
                                  area plot.
        @param small_tree_exp:  Inverse of the small-tree fixed area plot.
                                  eg. 100 = 1/100th acre plot
        @param break_dbh:  Break point diameter in inches. Any trees smaller
                             than this diameter were sampled using the
                             small-tree fixed area plots.
        @param sample_size:  Number of samples in the tree data
        @param not_stockable:  Number of non stocked samples
        @param sample_weight:  Stand sampling weight (acres) for aggregation post processors
        @param stockable_prop:  Percent of the stand considered stockable
        """
        KeywordBase.__init__(self, 'DESIGN', 'Sampling Design', format=0, **kargs)

        self.large_tree_baf = large_tree_baf
        self.small_tree_exp = small_tree_exp
        self.break_dbh = break_dbh
        self.sample_size = sample_size
        self.not_stockable = not_stockable
        self.sample_weight = sample_weight
        self.stockable_prop = stockable_prop

class SDICALC(KeywordBase):
    mindbh_reinke = DecimalField('Reinke Min. DBH')
    mindbh_zeide = DecimalField('Zeide Min. DBH')
    _method = CharacterField('SDI Method')

    def __init__(self, mindbh_reinke=0.0, mindbh_zeide=0.0
            , method='reinke', **kargs):
        """
        Select the method used to calculate Stand Density Index.
        
        @param mindbh_reinke: Min. DBH used in the calculating Reinke SDI.
        @param mindbh_zeide: Min. DBH used in the calculating Zeide SDI.
        @param method: Method used to calculate SDI, 'reinke' or 'zeide'.
        """
        KeywordBase.__init__(self, 'SDICALC', 'SDI Calculation Method', **kargs)

        self.mindbh_reinke = mindbh_reinke
        self.mindbh_zeide = mindbh_zeide
        self._method = {'zeide':1, 'reinke':0}[method]

class SDIMAX(KeywordBase):
    spp = CharacterField('Species')
    max_sdi = IntegerField('Max SDI')
    foo_1 = NullField()
    foo_2 = NullField()
    mort_dens = IntegerField('Mortality Density')
    max_dens = IntegerField('Maximum Density')
    stagnation = BooleanField('Stagnation Flag')

    def __init__(self, spp='ALL', max_sdi=0
            , mort_dens=55, max_dens=85, stagnation=0
            , **kargs):
        """
        Set the maximum stand density index, affecting density related mortality
        
        @param spp:  Species this SDI applies to
        @param max_sdi:  Maximum stand density index
        @param mort_dens:  Percent of max sdi where density related mortality occurs
        @param max_dens:  Percent of max sdi where stand reaches actual maximum
        @param stagnation:  Stagnation indicator for CR variant
        """
        KeywordBase.__init__(self, 'SDIMAX', 'Maximum Stand Density Index'
                             , format=0, **kargs)

        self.spp = spp
        self.max_sdi = max_sdi
        self.mort_dens = mort_dens
        self.max_dens = max_dens
        self.stagnation = stagnation
        self.kargs = kargs

class MORTMULT(KeywordBase):
    cycle = IntegerField('First Cycle')
    spp = CharacterField('Species')
    multiplier = DecimalField('Multiplier', precision=4)
    min_dbh = DecimalField('Min DBH')
    max_dbh = DecimalField('Max DBH')

    def __init__(self, cycle=1, spp='ALL', multiplier=1.0
            , min_dbh=0.0, max_dbh=999.9, **kargs
            ):
        """
        Adjust the predicted background mortality rate. 

        @param cycle:  First cycle the mortality multiplier applies to
        @param spp:  Species this mortality multiplier applies to
        @param multiplier:  Mortality multiplier, 1 + additional mortality rate
        @param min_dbh:  Minimum DBH the mortality multiplier applies to
        @param max_dbh:  Maximum DBH the mortality multiplier applies to
        """
        KeywordBase.__init__(self, 'MORTMULT', 'Background Mortality Multiplier'
                         , format=4, **kargs)

        self.cycle = cycle
        self.spp = spp
        self.multiplier = multiplier
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh

class STATS(KeywordBase):
    significance = DecimalField('Significance Level', precision=3)
    def __init__(self, significance=0.05, **kargs):
        """
        Sends stand inventory statistics report to the standard output file 

        @param significance:  Significance level for computing confidence intervals
        """
        KeywordBase.__init__(self, 'STATS', 'Compute Inventory Statistics'
                            , format=0, **kargs)

        self.significance = significance

class GROWTH(KeywordBase):
    inc_methods = {0:'Increment core IB, DBH is current'
                   , 1:'Remeasure OB'
                   , 2:'Increment core IB, DBH is prior'
                   , 3:'Remeasure OB, DBH is prior'}
    dbh_method = EnumField('DBH Method', default=0, enum_vals=inc_methods, allow_null=False)
    dbh_period = IntegerField('DBH Period')
    ht_method = IntegerField('Height Method')
    ht_period = IntegerField('Height Period')
    mort_period = IntegerField('Mortality Period')

    def __init__(self, dbh_method=0, dbh_period=10
                 , ht_method=0, ht_period=10, mort_period=10
                 , **kargs
                 ):
        """
        Define the growth measurement methods
        
        @param dbh_method:  Diameter growth measurement method:
                                0 - Direct inside bark, increment core
                                1 - Remeasurement outside bark, DBH is at the 
                                    end of the growth period, DG is DBH at the 
                                    beginning of the growth period.
                                2 - Direct inside bark, increment core,
                                    ouside bark DBH is at the beginning of the 
                                    growth period
                                3 - Remeasurement outside bark, DBH is at the 
                                    beginning of the growth period, DG is DBH at 
                                    the end of the growth period
        @param dbh_period:  Years represented by the dbh increment
        @param ht_method:  Height growth measurement method
        @param ht_period:  Years represented by the height increment
        @param mort_period:  Years represented by the mortality observation
        """
        KeywordBase.__init__(self, 'GROWTH', 'Define the Calibration Measurements'
                         , format=0, **kargs)

        self.dbh_method = dbh_method
        self.dbh_period = dbh_period
        self.ht_method = ht_method
        self.ht_period = ht_period
        self.mort_period = mort_period

# class ECHOSUM(KeywordSet):
#    def __init__(self, file_path, file_num=51, file_status=None, **kargs):
#        """
#        Output the summary table to a file.
#
#        Combines and ECHOSUM keyword with OPEN to enforce a known file path
#
#        @param file_path:  Output file path
#        @param file_num:  Output file reference number
#        @param file_status:
#        """
#        KeywordSet.__init__(self, **kargs)
#        self.title = 'ECHOSUM'
#
#        self.file_path = file_path
#        self.file_num = file_num
#        self.file_status = file_status
#
#        k = OPEN(file_path=self.file_path, file_num=self.file_num
#                 , status=self.file_status)
#        self.append(k)
#
#        k = KeywordBase('ECHOSUM', add_vals=(file_num,), format=0)
#        self.append(k)

class ECHOSUM(KeywordBase):
    file_num = IntegerField('File Number')

    def __init__(self, file_num=4, **kargs):
        """
        Output the summary table to a separate file
        
        @param file_num:  File number to write the summary table to
        """
        KeywordBase.__init__(self, 'ECHOSUM', 'Output Summary Table to a File'
                             , format=0, **kargs)

        self.file_num = file_num

class RANNSEED(KeywordBase):
    seed = IntegerField('Seed Number')

    def __init__(self, seed=55329, **kargs):
        """
        Reseed the random number generator
        
        @param seed:  Replacement pseudorandom number seed
        """
        KeywordBase.__init__(self, 'RANNSEED', 'Random Number Seed'
                             , format=0, **kargs)

        self.seed = seed

class NOHTDREG(KeywordBase):
    species = CharacterField('Species Code')
    suppress = BooleanField('Suppress Ht. Regression')
    def __init__(self, species='ALL', suppress=0, **kargs):
        """
        Suppress or enable height regression
        
        @param species:  Species affected
        @param suppress: 0-suppress; 1-enable
        """
        KeywordBase.__init__(self, 'NOHTDREG', 'Suppress Height Regression'
                             , format=0, **kargs)

        self.species = species
        self.suppress = suppress

class NOCALIB(KeywordBase):
    species = CharacterField('Species Code')

    def __init__(self, species='ALL', **kargs):
        """
        Suppress calculation of growth calibration
        
        @param species:  Species to suppress calibration for
        """
        KeywordBase.__init__(self, 'NOCALIB', 'Suppress Calibration Statistics'
                             , format=0, **kargs)

        self.species = species

# class CALBSTAT(KeywordBase):
#    file_num = IntegerField('File Number')
#    dbh_recs = IntegerField('DBH Records')
#    ht_recs = IntegerField('Height Records')
#
#    def __init__(self, file_num=13, dbh_recs=5, ht_recs=5, **kargs):
#        """
#        Request calibration of growth scaling factors
#
#        @param file_num:  Output file number
#        @param dbh_recs:  Minimum number of diameter growth samples required
#        @param ht_recs:  Minimum number of height growth samples required
#        """
#        KeywordBase.__init__(self, 'CALBSTAT', 'Compute Growth Scaling Factors'
#                             , format=5, **kargs)
#
#        self.file_num = file_num
#        self.dbh_recs = dbh_recs
#        self.ht_recs = ht_recs

class CALBSTAT(KeywordSet):
    def __init__(self, file_path, file_num=13
                 , dbh_recs=5, ht_recs=5, **kargs):
        """
        Output the calibration statistics table to a file.
        
        Combines and CALBSTAT keyword with OPEN to enforce a known file path
        
        @param file_path:  Output file path  
        @param file_num:  Output file reference number
        @param dbh_recs:  Minimum number of DBH increment records required for calibration
        @param ht_recs:  Minimum number of height increment records required for calibration
        """
        KeywordSet.__init__(self, **kargs)
        self.title = 'Calibration Statistics'

        self.file_path = file_path
        self.file_num = file_num
        self.dbh_recs = dbh_recs
        self.ht_recs = ht_recs

#        k.name = ''
#        k.comment = ''


        k = KeywordBase('CALBSTAT', add_vals=(self.file_num
                                              , self.dbh_recs
                                              , self.ht_recs)
                        , format=0)
        self.append(k)
        k = OPEN(file_path=self.file_path, file_num=self.file_num)
        k.name = ''
        k.comment = ''
        self.append(k)

class OPEN(KeywordBase):

    file_path = TextField('File Path')
    file_num = IntegerField('File Reference Number')
    null_zero = IntegerField('Null Zero')
    status = IntegerField('File Status')
    max_len = IntegerField('Max Length')
    formatted = IntegerField('Formatted')

    __supplemental__ = ('file_path',)

    def __init__(self, file_path=None, file_num=None
                 , null_zero=None, status=None
                 , max_len=None, formatted=None, **kargs):
        """
        Open a file for I/O

        @param file_path:  File path
        @param file_num:  FVS file reference number
        @param null_zero:  Null values as zero
        @param status:  Status ???
        @param max_len:  Maximum line length
        @param formatted:  Formatted ???
        """
        KeywordBase.__init__(self, 'OPEN', 'Open File for IO'
                             , format=KW_FMT_ONELINE_STACKED, **kargs)

        self.file_path = file_path
        self.file_num = file_num
        self.null_zero = null_zero
        self.status = status
        self.max_len = max_len
        self.formatted = formatted

class SPGROUP(KeywordBase):
    group_name = CharacterField('Species Group')
    spp_list = TextField('Species List')
    __supplemental__ = ('spp_list',)

    def __init__(self, group_name, spp_list=[], **kargs):
        """
        Species Group

        @param group_name:  Moniker given to the species group
        @param spp_list:  Python iterable of FVS Species codes 
        """
        # init the Keyword parent
        KeywordBase.__init__(self, 'SPGROUP', 'Species Group'
                             , format=KW_FMT_ONELINE_STACKED, **kargs)

        self.group_name = group_name
        if isinstance(spp_list, (str,)):
            self.spp_list = spp_list
        else:
            self.spp_list = ' '.join(spp_list)

class BFFDLN(KeywordBase):
    species = CharacterField('Species')
    intercept = DecimalField('Intercept', precision=6)
    slope = DecimalField('Slope', precision=6)

    def __init__(self, species='ALL', intercept=0.0, slope=1.0, **kargs):
        """
        Board Foot Volume Defect Log Linear Equation
        
        @param species:  Affected single species or species group
        @param intercept:  Log linear equation intercept
        @param slope:  Log linear equation slope
        """
        KeywordBase.__init__(self, 'BFFDLN', 'Board Foot Defect Equation'
                         , format=KW_FMT_ONELINE
                         , **kargs)

        self.species = species
        self.intercept = intercept
        self.slope = slope

class VOLEQNUM(KeywordBase):
    species = CharacterField('Species')
    cuft_eq = CharacterField('Cubic Foot Equation')
    bdft_eq = CharacterField('Board Foot Equation')
    
    def __init__(self, species='ALL', cuft_eq='', bdft_eq='', **kargs):
        """
        Sets the volume equation number used to calculate volume.
        
        @param species:  Species code for these equation numbers
        @param cuft_eq:  Cubic Foot NVEL equation number.
        @param bdft_eq:  Board Foot NVEL equation number
        """
        KeywordBase.__init__(self, 'VOLEQNUM', 'Volume Equation Numbers'
                         , format=KW_FMT_ONELINE
                         , **kargs)

        self.species = species
        self.cuft_eq = cuft_eq
        self.bdft_eq = bdft_eq
        
class BFVOLEQU(KeywordBase):
    species = CharacterField('Species')
    transition_code = IntegerField('Transition Code')
    transition_size = DecimalField('Transition Size', precision=4)
    __supplemental__ = ('coefficients',)

    def __init__(self, species='ALL', transition_code=0, transition_size=20.5
            , coefficients_1=[], coefficients_2=[], **kargs):
        """
        Board Foot Volume Defect Log Linear Equation
        
        @param species:  Affected single species or species group
        @param transition_code:  Tree size coefficient transition code
        @param slope:  Tree size coefficient transition DBH.
        """
        KeywordBase.__init__(self, 'BFVOLEQU', 'Board Foot Volume Equation'
                         , format=KW_FMT_ONELINE_STACKED
                         , **kargs)

        self.species = species
        self.transition_code = transition_code
        self.transition_size = transition_size
        self.coefficients_1 = coefficients_1
        self.coefficients_2 = coefficients_2

    @property
    def coefficients(self):
        c = ''.join('{:>10g}'.format(v) for v in self.coefficients_1)
        c += '\n' + ''.join('{:>10g}'.format(v) for v in self.coefficients_2)
        return c

class CFVOLEQU(KeywordBase):
    species = CharacterField('Species')
    transition_code = IntegerField('Transition Code')
    transition_size = DecimalField('Transition Size', precision=4)
    __supplemental__ = ('coefficients',)

    def __init__(self, species='ALL', transition_code=0, transition_size=20.5
            , coefficients_1=[], coefficients_2=[], **kargs):
        """
        Cubic Foot Volume Defect Log Linear Equation
        
        @param species:  Affected single species or species group
        @param transition_code:  Tree size coefficient transition code
        @param slope:  Tree size coefficient transition DBH.
        """
        KeywordBase.__init__(self, 'CFVOLEQU', 'Cubic Foot Volume Equation'
                         , format=KW_FMT_ONELINE_STACKED
                         , **kargs)

        self.species = species
        self.transition_code = transition_code
        self.transition_size = transition_size
        self.coefficients_1 = coefficients_1
        self.coefficients_2 = coefficients_2

    @property
    def coefficients(self):
        c = ''.join('{:>10g}'.format(v) for v in self.coefficients_1)
        c += '\n' + ''.join('{:>10g}'.format(v) for v in self.coefficients_2)
        return c

class MCFDLN(KeywordBase):
    species = CharacterField('Species')
    intercept = DecimalField('Intercept', precision=6)
    slope = DecimalField('Slope', precision=6)

    def __init__(self, species='ALL', intercept=0.0, slope=1.0, **kargs):
        """
        Merchantable Cubic Foot Volume Defect Log Linear Equation
        
        @param species:  Affected single species or species group
        @param intercept:  Log linear equation intercept
        @param slope:  Log linear equation slope
        """
        KeywordBase.__init__(self, 'MCFDLN', 'Board Foot Defect Equation'
                         , format=0, **kargs)

        self.species = species
        self.intercept = intercept
        self.slope = slope

class SNAGPSFT(KeywordBase):
    species = CharacterField('Species')
    soft_prop = DecimalField('Soft Snag Proportion', precision=3)

    def __init__(self, species='ALL', soft_prop=0.0, **kargs):
        """
        Snag soft proportion

        @param species:  Snag species the proportion applies to
        @param soft_prop:  Ratio of soft snags to hard
        """
        KeywordBase.__init__(self, 'SNAGPSFT', 'Snag soft proportion'
                             , format=0, **kargs)

        self.species = species
        self.soft_prop = soft_prop

class COMPUTE(KeywordBase):
    """Compute Even Monitor Variables
    """
    cycle = IntegerField('Cycle')

    def __init__(self, cycle=0, **kargs):
        """
        Event Monitor Compute Variables

        @param cycle: Simulation cycle to the compute expressions for
        """
        KeywordBase.__init__(self, 'COMPUTE', 'Event Monitor Compute'
                             , format=3, **kargs
                             )

        self.cycle = cycle

class NOTREES(KeywordBase):
    """Indicates no projectable trees in the input
    """
    def __init__(self, **kargs):
        KeywordBase.__init__(self, 'NOTREES', 'No Tree Records')

class NOTRIPLE(KeywordBase):
    """Suppress Tree Record Tripling
    """
    def __init__(self, **kargs):
        KeywordBase.__init__(self, 'NOTRIPLE', 'Suppress Tree Record Tripling')

class FIXMORT(KeywordBase):
    """
    Apply a fixed mortality proportion to species tpa records.
    """
    period = IntegerField('Period')
    species = CharacterField('Species')
    proportion = DecimalField('Proportion', precision=4)
    min_dbh = DecimalField('Min DBH', precision=1)
    max_dbh = DecimalField('Max DBH', precision=1)
    effect = IntegerField('Effect')
    distribution = IntegerField('Distribution')

    def __init__(self, period=0, species='ALL', proportion=0.0
                 , min_dbh=0.0, max_dbh=999.0
                 , effect=0, distribution=0, **kargs):
        """
        Apply a fixed mortality proportion to species tpa records.
        
        @param period:  Period affected by the mortality proportion.
        @param species:  Affected species.
        @param proportion:  Fraction of tree killed during the affected periods.
        @param min_dbh:  Minimum DBH to apply the proportion to.
        @param max_dbh: Maximum DBH to apply the proportion to.
        @param effect: Effect of the proportion on mortality estimates.
        @param distribution: Distribution of mortality estimates amonge trees.
        """
        KeywordBase.__init__(self, 'FIXMORT', 'Apply a Fixed Mortality Proportion'
                             , format=0, **kargs)
        self.period = period
        self.species = species
        self.proportion = proportion
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh
        self.effect = effect
        self.distribution = distribution
        self.kargs = kargs

class STRCLASS(KeywordBase):
    """
    Compute and report structure classification statistics.
    """
    output = BooleanField('Write Reports')
    gap_size = IntegerField('Gap Size')
    sapling_max_dbh = DecimalField('Sapling Max DBH', precision=1)
    pole_max_dbh = DecimalField('Pole Max DBH', precision=1)
    stratum_min_cvr = IntegerField('Stratum Min Cover')
    bare_ground_max_trees = IntegerField('Bare Ground Max Trees')
    stem_exclusion_pct_sdi = IntegerField('Stem Exclusion Pct. SDI')

    def __init__(self
            , output=True, gap_size=30, sapling_max_dbh=5.0
            , pole_max_dbh=25.0, stratum_min_cvr=5
            , bare_ground_max_trees=200, stem_exclusion_pct_sdi=30
            , **kargs):
        """
        Compute and report structure classification statistics.
        
        @param output: If True the summary report is generated.
        @param gap_size: Percentage of tree height used to define a gap.
        @param sapling_max_dbh: DBH separating sapling from pole sized trees.
        @param pole_max_dbh: DBH separating pole from large sized trees.
        @param stratum_min_cvr: Minimum percentage cover for a stratum to be included.
        @param bare_ground_max_trees: Maximum number of trees for a stand to qualify as bare-ground.
        @param stem_exclusion_pct_sdi: Percentage of maximum SDI to classify a stand as stem exclusion.
        """
        KeywordBase.__init__(self
                , 'STRCLASS'
                , 'Compute and report the structure classification statistics.'
                , format=0, **kargs)

        self.output = output
        self.gap_size = gap_size
        self.sapling_max_dbh = sapling_max_dbh
        self.pole_max_dbh = pole_max_dbh
        self.stratum_min_cvr = stratum_min_cvr
        self.bare_ground_max_trees = bare_ground_max_trees
        self.stem_exclusion_pct_sdi = stem_exclusion_pct_sdi

class COMPRESS(KeywordBase):
    """
    Compress the treelist to reduce data overhead
    """
    period = IntegerField('Period')
    records = IntegerField('Records')
    diff_pct = IntegerField('Difference Percent')
    debug = IntegerField('Debug')

    def __init__(self, period=1, records=675, diff_pct=50, debug_out=0, **kargs):
        """
        Compress the treelist to reduce data overhead
        
        @param period:  Year or cycle to compress the treelist.
        @param records:  Number of records to compress the treelist into.
        @param diff_pct:  Percentage of new records that will use the largest 
                            difference between trees.
        @param debug: Output debug messages.
        """
        KeywordBase.__init__(self, 'COMPRESS', 'Compress the treelist.'
                             , format=0, **kargs)
        self.period = period
        self.records = records
        self.diff_pct = diff_pct
        self.debug = debug_out

class BAIMULT(KeywordBase):
    """
    Adjust the large tree basa area increment prediction by a percentage
    """
    period = IntegerField('Period')
    species = CharacterField('Species')
    multiplier = DecimalField('Multiplier', precision=4)

    def __init__(self, period=0, species='ALL', multiplier=1.0, **kargs):
        """
        Adjust the large tree basa area increment prediction by a percentage
        
        @param period:  Period affected by the mortality proportion.
        @param species:  Affected species.
        @param multiplier:  Basal area growth prediction percent adjustment
        """
        KeywordBase.__init__(self, 'BAIMULT', 'Adjust the large tree basal area prediction'
                             , format=0, **kargs)
        self.period = period
        self.species = species
        self.multiplier = multiplier

# TODO: Fix readcord
class READCORD(KeywordBase):
    """Readjust Correction for Diameter
    """
    def __init__(self, spp_cor={}, spp_seq=None, **kargs):
        """
        @param spp_cor:  Species diameter increment correction factors 
        """
        if not spp_seq:
            raise ValueError("spp_seq requires a list of species codes in FVS sequence order.")\

        KeywordBase.__init__(self, 'READCORD', 'Readjust Correction for Diameter')

        self.spp_cor = spp_cor
        self.spp_seq = spp_seq

        self.cor_flds = [1.0] * len(spp_seq)

        for i, spp in enumerate(self.spp_seq):
            self.cor_flds[i] = self.spp_cor.get(spp, 1.0)

    def __str__(self):
        x = 'READCORD\n'

        i = 0
        for f in self.cor_flds:
            x += '%10.6f' % f
            i += 1
            if i == 8:
                x += '\n'
                i = 0

        return x

#        spp_flds = {}
#        for spp,cor in self.spp_cor:
#            seq = spp_seq[spp]

class DEBUG(KeywordBase):
    """
    Print Debug Output
    """
    cycle = IntegerField('Cycle')
    spec_subs = BooleanField('Specific Subroutines')
    file_num = IntegerField('File Number')
    subroutines = CharacterField('Subroutines')
    __supplemental__ = ('subroutines',)

    # #TODO: make this a KeywordSet couple with an OPEN Keyword
    def __init__(self, cycle=0, spec_subs=False, file_num=16, subroutines='', **kargs):
        """
        Print Debug Outputs

        @param cycle:  Simulation cycle to print debug messages
        @param spec_subs:  Print messages only for the specified subroutines
        @param file_num:  File number to write messages to
        @param subroutines:  Space seperated list of subroutines, or tuple/list
        """
        KeywordBase.__init__(self, 'DEBUG', 'Print Debug Outputs'
                             , format=KW_FMT_ONELINE_STACKED
                             , **kargs
                             )

        # if subroutines were passed as a list, join them with spaces
        if isinstance(subroutines, (tuple, list)):
            subroutines = ' '.join(subroutines)

        self.cycle = cycle
        self.spec_subs = spec_subs
        self.file_num = file_num
        self.subroutines = subroutines

        # if subroutines are listed, assume the DEBUG messages should be restricted
        if self.subroutines:
            self.spec_subs = True

class MINHARV(KeywordBase):
    """
    Specify a minimum harvest volume to implement a thinning 
    """
    cycle = IntegerField('Cycle')
    min_mcuft = IntegerField('Minimum Merch. CuFt VPA')
    min_bdft = IntegerField('Minimum BdFt VPA')
    min_baa = IntegerField('Minimum BAA')
    min_tcuft = IntegerField('Minimum Total CuFt VPA')

    def __init__(self, cycle=1
            , min_mcuft=0, min_bdft=0
            , min_baa=0, min_tcuft=0
            , **kargs):
        """
        MINHARV - Minimum Acceptable Harvest Volume
        
        @param cycle: First year/cycle the minimum harvest applies.
        @param min_mcuft: Minimum merch. cubic foot volume per acre.
        @param min_bdft: Minimum board foot volume per acre. 
        @param min_baa: Minimum basal area per acre.
        @param min_tcuft: Minimum total cubic foot volume per acre.
        """

        KeywordBase.__init__(self, 'MINHARV', 'Minimum Harvest Threshold'
                                 , format=KW_FMT_ONELINE
                                 , **kargs
                                 )
        self.cycle = cycle
        self.min_mcuft = min_mcuft
        self.min_bdft = min_bdft
        self.min_baa = min_baa
        self.min_tcuft = min_tcuft

class CUTEFF(KeywordBase):
    """
    Global cutting efficiency default
    """
    efficiency = DecimalField('Cutting Efficiency', precision=2)

    def __init__(self, efficiency=1.0, **kargs):
        """
        CUTEFF - Global cutting efficiency default
        
        @param efficiency:
        """
        KeywordBase.__init__(self, 'CUTEFF', 'Global cutting efficiency'
                             , format=KW_FMT_ONELINE
                             , **kargs
                             )
        self.efficiency = efficiency

class THINBBA(KeywordBase):
    """
    Thin from below to a BA target
    """
    cycle = IntegerField('Cycle')
    target_ba = IntegerField('Target')
    cut_eff = DecimalField('Cutting Efficiency', precision=2)
    min_dbh = DecimalField('Minimum DBH', precision=1)
    max_dbh = DecimalField('Maximum DBH', precision=1)
    min_ht = IntegerField('Minimum Height')
    max_ht = IntegerField('Maximum Height')

    def __init__(self, cycle=1, target_ba=999, cut_eff=1.0
                 , min_dbh=0.0, max_dbh=999.9
                 , min_ht=0, max_ht=999
                 , **kargs):
        """
        THINBBA - Thin From Below to a BA Target
        
        @param cycle:
        @param target_ba:
        @param cut_eff:
        @param min_dbh:
        @param max_dbh:
        @param min_ht:
        @param max_ht:
        """
        KeywordBase.__init__(self, 'THINBBA', 'Thin From Below to a BA Target'
                             , format=KW_FMT_ONELINE
                             , **kargs
                             )
        self.cycle = cycle
        self.target_ba = target_ba
        self.cut_eff = cut_eff
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh
        self.min_ht = min_ht
        self.max_ht = max_ht

class THINSDI(KeywordBase):
    """
    Thin to specified SDI
    """
    cycle = IntegerField('Cycle')
    target = IntegerField('Target')
    cut_eff = DecimalField('Cutting Efficiency', precision=2)
    species = CharacterField('Target Species')
    min_dbh = DecimalField('Minimum DBH', precision=1)
    max_dbh = DecimalField('Maximum DBH', precision=1)
    cut_control = IntegerField('Cutting Control')

    def __init__(self, cycle=1, target=0.0, cut_eff=1.0
                 , species='0', min_dbh=0.0, max_dbh=999.9
                 , cut_control=0
                 , **kargs):
        """
        THINSDI - Thin to a Target SDI
        
        @param cycle:
        @param target:
        @param cut_eff:
        @param species: 
        @param min_dbh:
        @param max_dbh:
        @param cut_control:
        """
        KeywordBase.__init__(self, 'THINSDI', 'Thin to a target SDI'
                             , format=KW_FMT_ONELINE
                             , **kargs
                             )
        self.cycle = cycle
        self.target = target
        self.cut_eff = cut_eff
        self.species = species
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh
        self.cut_control = cut_control

class THINBTA(KeywordBase):
    """
    Thin from below to a TPA target
    """
    cycle = IntegerField('Cycle')
    target_tpa = IntegerField('Target')
    cut_eff = DecimalField('Cutting Efficiency', precision=2)
    min_dbh = DecimalField('Minimum DBH', precision=1)
    max_dbh = DecimalField('Maximum DBH', precision=1)
    min_ht = IntegerField('Minimum Height')
    max_ht = IntegerField('Maximum Height')

    def __init__(self, cycle=1, target_tpa=999, cut_eff=1.0
                 , min_dbh=0.0, max_dbh=999.9
                 , min_ht=0, max_ht=999
                 , **kargs):
        """
        THINBBA - Thin From Below to a TPA Target
        
        @param cycle:
        @param target_tpa:
        @param cut_eff:
        @param min_dbh:
        @param max_dbh:
        @param min_ht:
        @param max_ht:
        """
        KeywordBase.__init__(self, 'THINBTA', 'Thin From Below to a TPA Target'
                             , format=KW_FMT_ONELINE
                             , **kargs
                             )
        self.cycle = cycle
        self.target_tpa = target_tpa
        self.cut_eff = cut_eff
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh
        self.min_ht = min_ht
        self.max_ht = max_ht

def print_test():
    # #TODO: implement better testing

    print(''.join(['%10d' % i for i in range(1, 9)]))
    print('1234567890' * 8)

    #---STANDIDENT
    print(STDIDENT())
    # #TODO: this should truncate or raise and exception when width is exceeded
    x = STDIDENT('1066666660123_Thisisareallylongstandid', 'Simple stand description')
    print(x)

    #---STANDCN
    print(STANDCN())
    x = STANDCN('a987634123')
    print(x)
    print(x.__repr__())

    # --STDINFO
    print(STDINFO())
    x = STDINFO(637, 46, 99, 330, 55, 5, 122)
    print(x)
    print('    ', x.__repr__())

    #---INVYEAR
    print(INVYEAR())
    x = INVYEAR(2010)
    print(x)
    print('\t', x.__repr__())

    #---DESIGN
    print(DESIGN())
    x = DESIGN(42.1, 115, 8.0, 24, 3, 2.4, 0.85)
    print(x)
    print('\t', x.__repr__())

    #---SDIMAX
    print(SDIMAX())
    x = SDIMAX('DF', 600, 65, 95, True)
    print(x)
    print('\t', x.__repr__())

    # --MORTMULT
    print(MORTMULT())
    x = MORTMULT(5, 'RA', 0.85, 16.0, 999.9)
    print(x)
    print('\t', x.__repr__())

    #---STATS
    print(STATS())
    x = STATS(.32)
    print(x)
    print('\t', x.__repr__())

    #---GROWTH
    print(GROWTH())
    x = GROWTH(1, 5, 3, 20, 15)
    print(x)
    print('\t', x.__repr__())

    #---ECHOSUM
    print(ECHOSUM(99))
    x = ECHOSUM(99)
    print(x)
    print('\t', x.__repr__())

    #---NOCALIB
    print(NOCALIB())
    x = NOCALIB('RA')
    print(x)
    print('\t', x.__repr__())

    #---CALBSTAT
    print(CALBSTAT(r'c:\temp\calibration.txt'))
    x = CALBSTAT(r'c:\temp\calibration.txt', dbh_recs=1, ht_recs=2)
    print(x)
    print('\t', x.__repr__())

    #---SPGROUP
    print(SPGROUP('CONIFER', 'DF WH RC'))
    x = SPGROUP('CONIFER', ['DF', 'WH', 'RC'])
    print(x)
    print(x.__repr__())

    #---BFFDLN
    x = BFFDLN('CONIFER', .0123, 1.056)
    print(x)
    print(x.__repr__())

    #---MCFDLN
    x = MCFDLN('ALL', 1.023, .9876)
    print(x)
    print(x.__repr__())

    #---SNAGPSFT
    x = SNAGPSFT('RA', .912)
    print(x)
    print(x.__repr__())

    #---MANAGED
    print(MANAGED())
    x = MANAGED(2011, False)
    print(x)
    print(x.__repr__())

    #---SITECODE
    x = SITECODE('DF', 120)
    print(x)
    print(x.__repr__())

    #---SETSITE
    print(SETSITE())
    x = SETSITE(5, 16, 455, 'CONIFER', 95, True, 600)
    print(x)
    print(x.__repr__())

    #---MGMTID
    print(MGMTID())
    x = MGMTID('GROW')
    print(x)
    print(x.__repr__())

    #---COMPUTE
    x = COMPUTE(11)

    baa = eventmonitor.ExpressionBlock(title='BAA Compute')
    baa += 'PI = 3.14159265'
    baa += '_BAC = PI / 576.0'
    baa += 'BAA = TPA * DBH*DBH*_BAC'
#    x.children.append(baa)
    x += baa

    x += 'PCT_SDI = (QMD/10.0)^1.605 / MAX_SDI'

    x.name = 'COMPUTE Demo'
    x.comment = 'This is a demonstration of a long comment\non a COMPUTE block keyword.'
    print(x)
    print(x.__repr__())

    #---FFE_FMIN
    ffe = FFE_FMIN()
    ffe.comment = 'Fire and Fuels Keyword Block'

    hard_fuel = FFE_FUELINIT(fuel_3_6=25, fuel_12_20=55.5, fuel_duff=12.45)
    ffe += hard_fuel

    print(ffe)
    print(ffe.__repr__())

    x = TREEFMT('(I4,T1,I7,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,' \
                        '3(I2,I2),2I1,I2,2I3,2I1,F3.0)')
    print(x)
    print(TREEFMT(''))
    print(x.__repr__())

    #---TIMEINT
    y = TIMEINT()
    x = TIMEINT(0, 5)
    print(x)
    y.cycle = 99
    x.cycle = 4
    print('y', y)
    print(x.__repr__())

    #---AddHoc
    x = AddHocKeyword('ADDHOC', field_vals=('fld1', 1, 2, 3.75)
                      , supplemental=('this is a supplemental record', 'This is another')
                      , format=5)
    print(x)
    print(x.__repr__())

    return

    x = CYCLEAT(2009)
    x.year = 2012
    print(x)

    x = TIMEINT(1, 7)
    x.cycle = 4
    x.length = 3
    print(x)

    x = NUMCYCLE(10)
    x.cycles = 8
    print(x)

    x = TREELIST()
    x.firstCycle = 4
    x.fileNum = 3
    x.headerStyle = 0
    x.cycleZero = 1
    x.liveDead = 1
    x.dbhIncrement = 1
    print(x)

    x = CUTLIST()
    x.firstCycle = 4
    x.fileNum = 3
    x.headerStyle = 0
    x.rptFormat = 1
    print(x)

    x = MGMTID()
    x.mgmtId = 'yada'
    print(x)

    x = MANAGED()
    print(x)

    x = INVYEAR(1998)
    print(x)

    x = FFE_CARBREPT(1, 50, 10)
    print(x)

    x = DESIGN(20, 100, 6, 15, 2, 445, .87)
    print(x)

    x = DB_DATABASE()
    s = DB_SUMMARY()
    x.children.append(s)
    t = DB_TREELIST(2)
    x.children.append(t)
    print(x)

if __name__ == '__main__':

#     print_test()
    k = STDIDENT(stand_id='FooBar', description='This is Foo')
    print(k)
