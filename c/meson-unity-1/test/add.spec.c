#include <unity.h>

void test_should_work(void) {
  double x = 1.111111;
  double y = 2.222222;

  TEST_ASSERT_DOUBLE_WITHIN(0.000001, x + y, 3.333333);
}
