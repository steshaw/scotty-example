#include <stdio.h>
#include <unistd.h>

int main() {
  int numProcessorsConfigured = sysconf(_SC_NPROCESSORS_CONF);
  int numProcessorsOnline = sysconf(_SC_NPROCESSORS_ONLN);
  printf("processors configured = %d\n", numProcessorsConfigured);
  printf("processors online = %d\n", numProcessorsOnline);
}
