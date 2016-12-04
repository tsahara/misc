import pt_pb2


person = pt_pb2.Person()
person.name = 'John'
person.id = 1234

with open("person.saved", "wb") as f:
    f.write(person.SerializeToString())

with open("person.saved", "rb") as f:
    print('person.name = %s' % person.name)
    print('person.id   = %u' % person.id)

