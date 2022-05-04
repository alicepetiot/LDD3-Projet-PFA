multi: .force
	dune exec multi/multi.exe

solo: .force
	dune exec solo/solo.exe

champignon: .force
	dune exec champignon/champignon.exe

ia: .force
	dune exec ia/main.exe

server: .force
	./server &
	
GO_B/a:
	gcc GO_B/a.c -o GO_B/a
	
a: GO_B/a
	./GO_B/a
	
GO_B/b:
	gcc GO_B/b.c -o GO_B/b
	
b: GO_B/b
	./GO_B/b

#main.exe
#./_build/default/main.exe Tests/nom_du_test.extension 
test: 
	dune build @tests/runtest

.force:
