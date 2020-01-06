#include <Rcpp.h>
using namespace Rcpp;


// partition the array using last element as pivot
int partition (NumericVector arr, int low, int high) 
{ 
  int pivot = arr[high];    // pivot 
  int i = (low - 1);   
  
  for (int j = low; j <= high- 1; j++) 
  { 
    //if current element is smaller than pivot, increment the low element
    //swap elements at i and j
    if (arr[j] <= pivot) 
    { 
      i++;    // increment index of smaller element 
      // swap i and j 
      int t = arr[i]; 
      arr[i] = arr[j]; 
      arr[j] = t;
    } 
  } 
  //swap i+1 and high
  int t = arr[i+1]; 
  arr[i+1] = arr[high]; 
  arr[high] = t; 
  
  return (i + 1);
} 

NumericVector quickOrd(NumericVector arr, int w, int p, int r) 
{
  if (p < r) 
  { 
    int q = partition(arr, p, r);
    if(w < q){
    //sort the sub arrays independently 
      return quickOrd(arr, w, p, q - 1);
    }
    else if(w > q) {
      return quickOrd(arr, w, q + 1, r);
    }
    else
      return arr[q];
  }
} 

//[[Rcpp::export]]
NumericVector order_statistic(NumericVector x, int w){
  return quickOrd(x, w - 1, 0, x.size()-1);
}

