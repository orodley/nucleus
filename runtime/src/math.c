#include <math.h>
#include "nuc.h"

extern inline nuc_val float_to_nuc_val(float f);
extern inline float nuc_val_to_float(nuc_val x);

nuc_val rt_log10(nuc_val flt_val)
{
	// TODO: type checking
	
	float x = nuc_val_to_float(flt_val);
	return float_to_nuc_val(log10f(x));
}

nuc_val rt_floor(nuc_val flt_val)
{
	// TODO: type checking
	
	float x = nuc_val_to_float(flt_val);
	return INT_TO_NUC_VAL((int)floorf(x));
}

nuc_val rt_ceiling(nuc_val flt_val)
{
	// TODO: type checking
	
	float x = nuc_val_to_float(flt_val);
	return INT_TO_NUC_VAL((int)ceilf(x));
}

nuc_val rt_int_to_float(nuc_val int_val)
{
	// TODO: type checking
	
	int x = NUC_VAL_TO_INT(int_val);
	return float_to_nuc_val((float)x);
}
