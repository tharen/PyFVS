"""
Classes to represent event monitor functions and routines

Created on Nov 18, 2013

@author: THAREN
"""

import logging

log = logging.getLogger('fvs_keywords.evenmonitor')

class ExpressionBlock(list):
    def __init__(self, title='', comment='', items=[], **kargs):
        """
        Represent a block of event  
        """
        list.__init__(self, items)
        self.title = title
        self.comment = comment
        self.rank = 0

    def __iadd__(self, other):
        self.append(other)
        return self

    def __str__(self):
        s = ''

        if self.title:
            s += '\n\n! Expression Block: %s\n' % self.title

        if self.comment:
            lines = self.comment.strip().split('\n')
            if len(lines) > 1:
                s += '!*******************\n'
                for line in lines:
                    s += '!  %s\n' % line
                s += '!*******************\n'

            else:
                s += '! %s\n' % self.comment

        for item in self:
            s += '%s\n' % item

        if self.title:
            s += '! End: %s' % self.title.strip()

        return s.strip()

class EventFunction(object):
    func_name = 'foo'
    parms = ''  # comma seperated string of named format markers

    def __init__(self, variable_name):
        """
        Event monitor compute function.
        """
        self.variable_name = variable_name

    def __str__(self):
        parms = self.parms.format(**self.__dict__)
        s = '{}={}({})'.format(self.variable_name, self.func_name, parms)
        return s

class IF(object):
    def __init__(self, wait_time=999, variables=None, condition=None
            , activities=None, title='', comment=''
            , *args, **kargs):
        """
        FVS IF-THEN block
        """
        self.wait_time = wait_time
        self.variables = variables
        self.condition = condition
        self.activities = activities
        self.title = title
        self.comment = comment

    def __str__(self):
        if not self.condition:
            log.warn('IF-THEN block without a condition, "{}"'.format(self.title))
            self.condition = '1 EQ 1'

        if not self.activities:
            log.warn('IF-THEN block without activities, "{}"'.format(self.title))

        s = ''

        if self.title:
            s += '*Begin: %s\n' % self.title.strip()

        if self.comment:
            s += 'COMMENT\n%s\nEND\n' % self.comment.strip()

        if self.variables:
            s += '{}'.format(self.variables)

        s += '{:<10s}{:>10d}\n'.format('IF', self.wait_time)
        s += '{}\n'.format(self.condition)
        s += 'THEN\n'
        s += '{}\n'.format(str(self.activities).rstrip())
        s += 'ENDIF\n'

        if self.title:
            s += '*End: %s\n' % self.title.strip()

        return s

class AlgebraExpression(object):
    # #TODO: expression should be dynamic, possibly consuming EventFunctions
    def __init__(self, lhs, rhs):
        """
        Defines an event monitor compute variable as an algebraic expression.
        """
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        s = '{lhs}={rhs}'.format(**self.__dict__)
        return s

class LogicalExpression(object):
    # #TODO: expression should be dynamic, possibly consuming EventFunctions
    def __init__(self, variable_name=None, expression=None):
        """
        Defines an event monitor compute variable as a logical expression.
        """
        self.variable_name = variable_name
        self.expression = expression

    def __str__(self):
        s = '{expression}'
        if self.variable_name:
            s = '{variable_name}=' + s
        return s.format(**self.__dict__)

class SPMCDBH(EventFunction):
    func_name = 'SPMCDBH'
    parms = ('{statistic},{species},{tree_value:d}'
            ',{min_dbh:.1f},{max_dbh:.1f},{min_ht:.1f}'
            ',{max_ht:.1f},{tree_status},{point_num}')

    def __init__(self
            , variable_name, statistic, species='', tree_value=0
            , min_dbh=0.0, max_dbh=999.9, min_ht=0.0, max_ht=999.9
            , tree_status=0, point_num=0.0):
        """
        Compute function to generate a filtered stand summary variable
        """
        EventFunction.__init__(self, variable_name)
        self.statistic = statistic
        self.species = species
        self.tree_value = tree_value
        self.min_dbh = min_dbh
        self.max_dbh = max_dbh
        self.min_ht = min_ht
        self.max_ht = max_ht
        self.tree_status = tree_status
        self.point_num = point_num

        if not self.species:
            self.species = 0
