package singleton

import "testing"

func TestGetInstance(t *testing.T) {
	counterRef1 := GetInstance()

	if counterRef1 == nil {
		t.Error("Expected Singleton instance, got nil")
	}

	currentCount := counterRef1.AddOne()

	if currentCount != 1 {
		t.Errorf("want %d for first call, got %d", 1, currentCount)
	}

	counterRef2 := GetInstance()
	if counterRef2 != counterRef1 {
		t.Error("Counters are not the same instance")
	}

	currentCount = counterRef2.AddOne()

	if currentCount != 2 {
		t.Errorf("want %d for second call, got %d", 2, currentCount)
	}
}
