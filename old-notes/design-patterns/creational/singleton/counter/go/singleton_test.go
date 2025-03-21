package singleton

import "testing"

func TestGetInstance(t *testing.T) {
	t.Run("create instance correctly", func(t *testing.T) {
		counter := GetInstance()

		if counter == nil {
			t.Error("Expected Singleton instance, got nil")
		}
	})

	t.Run("always returns he same reference", func(t *testing.T) {
		ref1 := GetInstance()
		ref2 := GetInstance()

		if ref1 != ref2 {
			t.Error("Counters are not the same reference to the same instance")
		}
	})

	t.Run("returns correct value on first call", func(t *testing.T) {
		count := GetInstance().AddOne()

		if count != 1 {
			t.Errorf("Want 1 for first call, got %d", count)
		}
	})

	t.Run("returns correct value on each subsequent call", func(t *testing.T) {
		counter := GetInstance()

		/*
		 * NOTE: Go seems to treat all tests and subtests as part of one
		 * program.  Thus, the value of the counter has already been
		 * incremented once from the previous subtest when we called
		 * AddOne() for the first time.
		 */
		_ = counter.AddOne()
		count := counter.AddOne()

		if count != 3 {
			t.Errorf("Want 3 for third call, got %d", count)
		}
	})
}
