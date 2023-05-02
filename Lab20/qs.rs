fn partition(arr: &mut [i32], low: usize, high: usize) -> usize {
    let pivot = arr[high];
    let mut i = low;

    for j in low..high {
        if arr[j] < pivot {
            arr.swap(i, j);
            i += 1;
        }
    }

    arr.swap(i, high);
    // Returns i 
    i
}

fn quick_sort_helper(arr: &mut [i32], low: usize, high: usize) {
    if low < high {
        let pi = partition(arr, low, high);
        //Saturating sub returns 0 or subtraction of two numbers
        quick_sort_helper(arr, low, pi.saturating_sub(1));
        quick_sort_helper(arr, pi + 1, high);
    }
}

fn quick_sort(arr: &mut [i32]) {
    let len = arr.len();

    if len == 0 {
        return;
    }

    quick_sort_helper(arr, 0, len - 1);
}

fn main() {
    let mut arr = [4, 2, 4, 2, 4,2];
    quick_sort(&mut arr);
    println!("{:?}", arr);
}
