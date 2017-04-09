class Node:
	arg1 = None
	arg2 = None
	op   = None

	def __init__(self, op, arg1, arg2):
		self.op = op;
		self.arg1 = arg1;
		self.arg2 = arg2;

		
	def __str__(self):
		ret = "Node: {op: " + str(self.op) + ",\narg1: " + str(self.arg1) + ",\narg2: " + str(self.arg2) + "}\n"
		return ret



class Terminal:
	value = None
	def __init__(self, value):
		self.value = value
	def __str__(self):
		return  str(self.value)


class StackWrapper:
	classObject = None
	stackValue = None

	def __init__(self, classObject, stackValue):
		self.classObject = classObject
		self.stackValue = stackValue

	def __str__(self):
		return str(self.classObject)
