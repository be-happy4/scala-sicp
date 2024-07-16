package org.behappy.sicp.lang


def and(b1: Boolean, b2: Boolean): Boolean = b1 && b2
def and(b0: Boolean, bs: Boolean*): Boolean = bs.fold(b0)(_ && _)

def or(b1: Boolean, b2: Boolean): Boolean = b1 || b2
def or(b0: Boolean, bs: Boolean*): Boolean = bs.fold(b0)(_ || _)

def not(b: Boolean): Boolean = !b

