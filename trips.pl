% sample graph
link(start, a).
link(a, b).
link(a, c).
link(c, d).
link(b, e).
link(e, f).
link(d, f).

type(start, onnxConv2D).
type(a, onnxRelu).
type(b, onnxConv2DTransposed).
type(e, onnxRelu).
type(c, onnxConv2DTransposed).
type(d, onnxRelu).
type(f, onnxAdd).

% onnx to scorpio mappings
mapped(onnxRelu, readPathRelu).
mapped(onnxRelu, writePathRelu).
mapped(onnxRelu, inMemRelu).

mapped(onnxConv2D, conv2D).

mapped(onnxConv2DTransposed, conv2DTranspose).

mapped(onnxAdd, inMemAdd).

% connected operations

connected(read, readPathRelu).
connected(read, conv2D).
connected(read, conv2DTranspose).
connected(read, bilinearUpsampling).
connected(read, undilate).
connected(read, memCopy).
connected(read, memFill).
connected(read, nearestNeighborUpsampling).
connected(read, elementWiseAdd).
connected(read, notchfill).

connected(readPathRelu, conv2D).
connected(readPathRelu, conv2DTranspose).
connected(readPathRelu, bilinearUpsampling).
connected(readPathRelu, undilate).
connected(readPathRelu, memCopy).
connected(readPathRelu, memFill).
connected(readPathRelu, nearestNeighborUpsampling).
connected(readPathRelu, elementWiseAdd).
connected(readPathRelu, notchfill).

connected(conv2D, dilate).
connected(conv2D, biasAdd).
connected(conv2D, writePathRelu).
connected(conv2D, inMemAdd).
connected(conv2D, write).
connected(conv2D, dilate).

connected(conv2DTranspose, dilate).
connected(conv2DTranspose, biasAdd).
connected(conv2DTranspose, writePathRelu).
connected(conv2DTranspose, inMemAdd).
connected(conv2DTranspose, write).
connected(conv2DTranspose, dilate).

connected(bilinearUpsampling, dilate).
connected(bilinearUpsampling, biasAdd).
connected(bilinearUpsampling, writePathRelu).
connected(bilinearUpsampling, inMemAdd).
connected(bilinearUpsampling, write).

connected(biasAdd, writePathRelu).
connected(biasAdd, absolute).
connected(biasAdd, maxPool2x2).
connected(biasAdd, inMemAdd).
connected(biasAdd, inMemRelu).
connected(biasAdd, write).

connected(absolute, maxPool2x2).
connected(absolute, inMemAdd).
connected(absolute, inMemRelu).
connected(absolute, write).

connected(inMemAdd, write).
connected(inMemAdd, inMemRelu).

connected(inMemRelu, write).

connected(dilate, biasAdd).
connected(dilate, writePathRelu).
connected(dilate, absolute).
connected(dilate, maxPool2x2).
connected(dilate, inMemAdd).
connected(dilate, inMemRelu).
connected(dilate, write).

connected(maxPool2x2, inMemAdd).
connected(maxPool2x2, write).

connected(undilate, conv2D).
connected(undilate, writePathRelu).
connected(undilate, inMemAdd).
connected(undilate, write).

connected(memCopy, writePathRelu).
connected(memCopy, maxPool2x2).
connected(memCopy, inMemAdd).
connected(memCopy, write).

connected(memFill, writePathRelu).
connected(memFill, inMemAdd).
connected(memFill, write).

connected(nearestNeighborUpsampling, writePathRelu).
connected(nearestNeighborUpsampling, inMemAdd).
connected(nearestNeighborUpsampling, write).

connected(elementWiseAdd, writePathRelu).
connected(elementWiseAdd, absolute).
connected(elementWiseAdd, inMemAdd).
connected(elementWiseAdd, write).

connected(notchfill, write).

connected(write, _) :- false.

path(A, A, [A]).
path(A, B, [A|Tail]) :-
    connected(A, X),
    path(X, B, Tail). 
