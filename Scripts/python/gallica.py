from gallipy import Resource, Ark


def save(either, filename):
    # Handle exceptions here
    if either.is_left:
        raise either.value
    # Otherwise we can safely unwrap its content.
    with open(filename, 'wb') as file:
        file.write(either.value)
    return either  # Enables method chaining.


my_ark = 'ark:/12148/bpt6k37596h'
# my_ark = 'ark:/12148/bpt6k5619759j'
# filename = 'bpt6k5619759j.pdf'
filename = '20210621004753'
# Async call: save(either, filename) is a callback method.
Resource(my_ark).content(startview=1, mode='pdf').map(lambda x: save(x, filename))
# Resource(my_ark).content(startview=1, nviews=10, mode='pdf').map(lambda x: save(x, filename))
