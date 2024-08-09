using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// Sort the numbers in D in increasing order (if ID = &#39;I&#39;) or
        /// in decreasing order (if ID = &#39;D&#39; ).
        /// </para>
        /// <para>
        /// Use Quick Sort, reverting to Insertion sort on arrays of
        /// size &lt;= 20. Dimension of STACK limits N to about 2**32.
        /// </para>
        /// </summary>
        /// <param name="id">
        /// [in] ID is CHARACTER*1.
        /// = &#39;I&#39;: sort D in increasing order;
        /// = &#39;D&#39;: sort D in decreasing order.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The length of the array D.
        /// </param>
        /// <param name="d">
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the array to be sorted.
        /// On exit, D has been sorted into increasing order
        /// (D(1) &lt;= ... &lt;= D(N) ) or into decreasing order
        /// (D(1) &gt;= ... &gt;= D(N) ), depending on ID.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_slasrt", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Slasrt(
            char id,
            int n,
            float* d);
    }
}
