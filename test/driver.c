#include <stdio.h>

extern int mymain(void);

int sum2(int a, int b) {
  return a + b;
}

int sum3(int a, int b, int c) {
  return a + b + c;
}

int sum4(int a, int b, int c, int d) {
  return a + b + c + d;
}

int sum5(int a, int b, int c, int d, int e) {
  return a + b + c + d + e;
}

int sum6(
    int a, int b, int c, int d, int e,
    int a0) {
  return a + b + c + d + e
      + a0;
}


int sum7(
    int a, int b, int c, int d, int e,
    int a0, int b0) {
  return a + b + c + d + e
      + a0 + b0;
}

int sum8(
    int a, int b, int c, int d, int e,
    int a0, int b0, int c0) {
  return a + b + c + d + e
      + a0 + b0 + c0;
}



int sum10(
    int a, int b, int c, int d, int e,
    int a0, int b0, int c0, int d0, int e0) {
  return a + b + c + d + e
      + a0 + b0 + c0 + d0 + e0;
}

int sum20(
    int a, int b, int c, int d, int e,
    int a0, int b0, int c0, int d0, int e0,
    int a1, int b1, int c1, int d1, int e1,
    int a2, int b2, int c2, int d2, int e2) {
  return a + b + c + d + e
      + a0 + b0 + c0 + d0 + e0
      + a1 + b1 + c1 + d1 + e1
      + a2 + b2 + c2 + d2 + e2;
}


int mymain2() {
	int a = 10;
	int b = 20;
	return sum2(a, b);
}
int mymain3() {
	int a = 10;
	int b = 20;
	int c = 30;
	return sum3(a, b, c);
}
int mymain4() {
	int a = 10;
	int b = 20;
	int c = 30;
	int d = 40;
	return sum4(a, b, c, d);
}
int mymain5() {
	int a = 10;
	int b = 20;
	int c = 30;
	int d = 40;
	int e = 50;
	return sum5(a, b, c, d, e);
}
int mymain6() {
	int a = 10;
	int b = 20;
	int c = 30;
	int d = 40;
	int e = 50;
	int a0 = 50;
	return sum6(a, b, c, d, e,
		 	a0);
}
int mymain7() {
	int a = 10, b = 20, c = 30, d = 40, e = 50;
	int a0 = 50, b0 = 50;
	return sum7(a, b, c, d, e,
		 	a0, b0);
}
int mymain8() {
	int a = 10, b = 20, c = 30, d = 40, e = 50;
	int a0 = 50, b0 = 50, c0 = 60;
	return sum8(a, b, c, d, e,
		 	a0, b0, c0);
}



int main(int argc, char **argv) {
  printf("%d\n", mymain());
  return 0;
}
