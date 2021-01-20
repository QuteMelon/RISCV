int add(int a, int b);
int sub(int a, int b){
	return a - b;
}

int main(int argc, char** argv) {
  int res = sub(2,3);
  return res == -1 ? 1 : -1;
}
