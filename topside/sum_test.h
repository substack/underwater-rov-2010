    while (1) {
        // sum test
        byte x = serial_rx() - 48;
        byte y = serial_rx() - 48;
        serial_tx(x + y);
    }
