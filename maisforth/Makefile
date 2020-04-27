TARGET = an601
# For ALTERA Quartus
FPGA_BIN = hex
# For Xinlinx:
# FPGA_BIN = bit

all:	$(TARGET).vhd $(TARGET).bin $(TARGET).$(FPGA_BIN)

clean:
	rm -f $(TARGET).vhd $(TARGET).bin $(TARGET).hex $(TARGET).bit 

$(TARGET).bin: meta601.f cras601.f targ601.f
	gforth meta601.f -e "ROMIMAGE bye . " < meta601.input

$(TARGET).mem: $(TARGET).bin
	perl ../tools/bin-to-mem.pl $(TARGET).bin

$(TARGET).bit: $(TARGET).mem
	data2mem -bm maisforth_rom_bd.bmm -bt my_system09.bit -bd $(TARGET).mem -o b final.bit

$(TARGET).vhd: $(TARGET).bin
	perl ../tools/bin-to-ramb16.pl $(TARGET).bin

$(TARGET).hex: $(TARGET).bin
	srec_cat $(TARGET).bin -binary -o $(TARGET).hex -intel -address-length=2

