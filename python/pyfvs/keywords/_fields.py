"""
Parent classes for FVS keyword representations
"""

import copy
import logging
# import operator

# the makeProperty function is a little Python object magic
# from ..keywords import _utils
# from ..fvs_keywords import keywords
# from pyfvs.fvs_keywords import keywords
# from ..fvs_keywords import eventmonitor

# initiate logging as prepared by fvs._utils
log = logging.getLogger('pyfvs.fvs_keywords._keywords')

debug = log.debug
info = log.info
warn = log.warn
error = log.error
exception = log.exception

# #TODO: use constants or not???
# Keyword field format constants
# --align right--
FLD_FMT_ALIGN_LEFT = False
FLD_FMT_ALIGN_RIGHT = True

# --upper case--
FLD_FMT_MIXED_CASE = False
FLD_FMT_UPPER_CASE = True

class KeywordFieldBase(object):
    """
    Base class for representing a FVS keyword field.
    """
    # NOTE: Most of this functionality could now be reimplemented using the builtin string formatting/templates
    # NOTE: Type checking could be handled with get/set property methods

    def __new__(cls, *args, **kargs):
#         """Not implemented"""
        new_inst = super(KeywordFieldBase, cls).__new__(cls,)
        return new_inst

    def __init__(self, name, default=None, width=10
                 , align_right=True, allow_null=True):
        """
        Keyword field representation
        
        @param name:  Descriptive name for the field
        @param default:  Default field value
        @param width:  Field string width
        @param align_right:  Right align the field string
        @param allow_null:  Field value may be None
        """
        self.name = name
        self.default = default
        self.width = width
        self.align_right = align_right
        self.allow_null = allow_null

        self.value = default

    def _cast(self, value):
        """
        Ensure that value is of the expected type.
        """
        return value

    def __setattr__(self, attr, value):
        # validate the "value" of the field by calling the cast function
        if attr == 'value':
            # Null is anything that evaluates to False, excluding zero
            #  empty strings, lists, dicts, None, etc.
            if ((value != 0 and not value) and not self.allow_null):
                raise AttributeError('%s.value can not be Null' % (self.name,))

            object.__setattr__(self, attr, self._cast(value))

        # all other attributes pass through
        else:
            object.__setattr__(self, attr, value)

    def __repr__(self):
        return str(self.value)

    def __str__(self):
        if self.value == None:
            val_str = ''
        else:
            val_str = '%s' % self.value

        if len(val_str) > self.width:
            raise ValueError('Field value to wide: "%s" is exceeds width of %d' %
                             (val_str, self.width))

        if self.align_right:
            fmt = '%%%ds' % self.width

        else:
            fmt += '%%-%ds' % self.width

        return fmt % val_str

class NullField(KeywordFieldBase):
    def __init__(self, name='', width=10, *args, **kargs):
        """
        Null placeholder keyword field
        """
        KeywordFieldBase.__init__(self, name, **kargs)

    def __repr__(self):
        return ''

    def __str__(self):
        return ' ' * self.width

class CharacterField(KeywordFieldBase):
    def __init__(self, name, upper_case=True, ignore_case=False, **kargs):
        """
        Character string keyword field
        """
        KeywordFieldBase.__init__(self, name, **kargs)
        self.upper_case = upper_case
        self.ignore_case = ignore_case

    def _cast(self, value):
        return str(value)

    def __str__(self):
        if self.value == None:
            return ' ' * self.width

        val = self.value
        if self.ignore_case:
            val = self.value

        else:
            if self.upper_case:
                val = self.value.upper()

            else:
                val = self.value.lower()

        if self.align_right:
            fmt = '%%%ds' % self.width
        else:
            fmt = '%%-%ds' % self.width

        return fmt % val

class TextField(KeywordFieldBase):
    def __init__(self, name, width=160, **kargs):
        """
        Character string keyword field
        """
        KeywordFieldBase.__init__(self, name, width=width, **kargs)

    def _cast(self, value):
        return str(value)

    def __str__(self):
        if self.value == None:
            return ''

        val = self.value
        s = ''

        # Lines in an FVS keyword file cannot exceed 80 characters
        while len(val) > 80:
            s += val[:80]
            val = val[80:]

        s += val
        return s

class IntegerField(KeywordFieldBase):
    def __init__(self, name, **kargs):
        """
        Integer keyword field
        """
        KeywordFieldBase.__init__(self, name, **kargs)

    def _cast(self, value):
        if value is None:
            return value

        return int(value)

    def __str__(self):
        if self.value == None:
            return ' ' * self.width

        if self.align_right:
            fmt = '%%%dd' % self.width
        else:
            fmt = '%%-%dd' % self.width

        return fmt % self.value

class DecimalField(KeywordFieldBase):
    # #TODO: store with true precision using the decimal module, etc.
    def __init__(self, name, precision=1, **kargs):
        """
        Decimal type keyword field
        """
        KeywordFieldBase.__init__(self, name, **kargs)
        self.precision = int(precision)

    def _cast(self, value):
        if value is None:
            return value

        return float(value)

    def __str__(self):
        if self.value == None:
            return ' ' * self.width

        if self.align_right:
            fmt = '%%%d.%df' % (self.width, self.precision)

        else:
            fmt = '%%-%d.%df' % (self.width, self.precision)

        return fmt % self.value

class BooleanField(KeywordFieldBase):
    def __init__(self, name, **kargs):
        """
        Boolean keyword field
        """
        KeywordFieldBase.__init__(self, name, **kargs)

    def _cast(self, value):
        if value is None:
            return value

        return bool(value)

    def __str__(self):
        if self.value == None:
            return ' ' * self.width

        if self.align_right:
            fmt = '%%%dd' % self.width
        else:
            fmt = '%%-%dd' % self.width

        return fmt % self.value

class EnumField(KeywordFieldBase):
    def __init__(self, name, enum_vals={}, **kargs):
        """
        Restrict field values to a dictionary of enumerated values
        
        @param name:  Field name
        @param enum_vals:  Dictionary of allowable field values  
        """
        self.enum_vals = enum_vals

        KeywordFieldBase.__init__(self, name, **kargs)

    def __setattr__(self, attr, value):

        # validate the value against the enumerated values
        if attr == 'value':
            if not value in self.enum_vals:
                raise AttributeError('%s is not an enumerated value for the field' % value)

        # all others and enumerated values pass through
        super(EnumField, self).__setattr__(attr, value)
