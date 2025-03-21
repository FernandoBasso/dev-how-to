#include <stdio.h>
#include "list_ops.h"

/*
 * All operations always return a new list without modifying the
 * input list.
 */

list_t *new_list(size_t len, list_element_t elmts[]) {
  list_t *list = malloc(sizeof(list_t) + sizeof(list_element_t) * len);
  list->length = len;

  /* Sometimes we'll have a length > 0 but will add elements later
   * by some other means, like concatenating two lists. It means
   * length will be > 0 but elmts will be empty or NULL. */
  if (!elmts)
    return list;

  for (size_t i = 0; i < len; ++i)
    list->elements[i] = elmts[i];

  return list;
}

void delete_list(list_t *list) {
  free(list);
}

/**
 * Appends the second list to the first.
 *
 * • T.C: O(n).
 * • S.C: O(n).
 */
list_t *append_list(list_t *xs, list_t *ys) {
  size_t new_list_len = xs->length + ys->length;

  list_t *list = new_list(new_list_len, NULL);

  size_t i,
         j = 0;

  if (xs->length > 0)
    for (i = 0; i < xs->length; ++i, ++j)
      list->elements[j] = xs->elements[i];

  /* j is not reset to 0 as we want to keep adding elements from ys
   * right after the elements from xs. */

  if (ys->length > 0)
    for (i = 0 ; i < ys->length; ++i, ++j)
      list->elements[j] = ys->elements[i];

  return list;
}

/**
 * Returns a new list containing only elements that satisfy the
 * predicate.
 *
 * Loops over all elements twice.
 *
 * • T.C: O(n * 2).
 * • S.C: O(n).
 */
list_t *filter_list(list_t *list, bool (*filter)(list_element_t)) {
  list_t *filtered;

  size_t i,
         len = 0,
         j = 0;

  /* Count how many elements will pass the predicate so we can
   * allocate the correct amount of memory to the new list which
   * will contain the filtered elements. */
  for (i = 0; i < list->length; ++i)
    if(filter(list->elements[i]))
      ++len;

  filtered = new_list(len, NULL);

  for (i = 0; i < list->length; ++i)
    if (filter(list->elements[i]))
      filtered->elements[j++] = list->elements[i];

  return filtered;
}

/**
 * Returns the length of the list.
 *
 * • T.C: O(1).
 * • S.C: O(1).
 */
size_t length_list(list_t *list) {
  return list->length;
}

/**
 * Returns a new list with each element transformed by the map function.
 *
 * • T.C: O(n).
 * • S.C: O(n).
 */
list_t *map_list(list_t *list, list_element_t (*map)(list_element_t)) {
  list_t *mapped = new_list(list->length, NULL);
  size_t i;

  for (i = 0; i < list->length; ++i)
    mapped->elements[i] = map(list->elements[i]);

  return mapped;
}

/**
 * Folds (reduces) the given list from the left with a function.
 *
 * • T.C: O(n).
 * • S.C: O(n).
 */
list_element_t foldl_list(list_t *list, list_element_t initial,
                          list_element_t (*foldl)(list_element_t,
                                                  list_element_t)) {
  list_element_t acc = initial;
  size_t i;

  for (i = 0; i < list->length; ++i)
    acc = foldl(list->elements[i], acc);

  return acc;
}

/**
 * Folds (reduces) the given list from the right with a function.
 *
 * • T.C: O(n).
 * • S.C: O(n).
 */
list_element_t foldr_list(list_t *list, list_element_t initial,
                          list_element_t (*foldr)(list_element_t,
                                                  list_element_t)) {

  list_element_t acc = initial;
  size_t i;

  /* FIXME: Because we are using size_t (which means i cannot possibly
   * be negative), i >= 0 will always be true. Thus, we do i >= 1 and
   * index the elements with i - 1 to work around the problem. Maybe
   * there is a better way to solve this. */
  for (i = list->length; i >= 1; --i)
    acc = foldr(list->elements[i - 1], acc);

  return acc;
}

/**
 * Returns a new list with the reversed elements of the input list.
 *
 * • T.C: O(n).
 * • S.C: O(n).
 */
list_t *reverse_list(list_t *list) {
  list_t *reversed = new_list(list->length, NULL);
  size_t i,
         j = 0;

  /* FIXME: Same as the fixme above. Because i is size_t (non-negative),
   * comparing >= 0 is always true. Thus the i - 1 thing and i starts
   * one past the end of the list, instead if i = list->length - 1,
   * which would be the normal way of doing it and avoid the i - 1. */
  for (i = list->length; i >= 1; --i, ++j)
    reversed->elements[j] = list->elements[i - 1];

  return reversed;
}
