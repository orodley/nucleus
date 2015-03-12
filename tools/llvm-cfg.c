#include <stdio.h>

#include <llvm-c/Analysis.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/Support.h>

int main(int argc, char *argv[])
{
	if (argc != 3) {
		fprintf(stderr, "Usage: %s <bitcode file> <function name>\n", argv[0]);
		return 1;
	}

	char *bitcode_filename = argv[1];
	char *function_name = argv[2];

	LLVMMemoryBufferRef buf = NULL;
	char *msg = NULL;
	if (LLVMCreateMemoryBufferWithContentsOfFile(bitcode_filename, &buf, &msg)) {
		fprintf(stderr, "Error reading bitcode file '%s': %s\n",
				bitcode_filename, msg);
		return 2;
	}

	LLVMModuleRef mod = NULL;
	if (LLVMParseBitcode(buf, &mod, &msg)) {
		fprintf(stderr, "Error reading bitcode file '%s': %s\n",
				bitcode_filename, msg);
		return 3;
	}

	LLVMValueRef func = LLVMGetNamedFunction(mod, function_name);
	if (func == NULL) {
		fprintf(stderr, "No such function '%s' in module\n", function_name);
		return 4;
	}

	LLVMViewFunctionCFG(func);

	return 0;
}
