#include <iostream>
#include <cstring>

int main(void) {
	char s2[] = "ALIVE";
	if (!(std::strcmp("ALIVE",s2)))
		std::cout << "They are the same" << std::endl;
	else
		std::cout << "DIFFERENT" << std::endl;

	return 0;
}
	
