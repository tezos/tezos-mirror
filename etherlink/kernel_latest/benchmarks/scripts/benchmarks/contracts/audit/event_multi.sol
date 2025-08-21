// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract event_multi {
    event LogString(address indexed sender, string message);

    function event_string_short(uint256 runs) public {
        for (uint256 i=0; i < runs; i++) {
            emit LogString(msg.sender, "Hello World!");
        }

    }

    function event_string_long(uint256 runs) public {
        for (uint256 i=0; i < runs; i++) {
            // 1024 bytes
            emit LogString(msg.sender, "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed nisl ligula, mattis sed sollicitudin in, egestas quis lacus. Aenean vitae enim dapibus, tempor mi vitae, tristique ipsum. Fusce ac blandit sem. Suspendisse efficitur vel purus ut dignissim. Suspendisse faucibus eros sodales, tempus nisl id, iaculis arcu. Aliquam pulvinar sapien vel elit tincidunt dapibus. Nunc in lobortis urna, sed molestie lacus. Donec efficitur tortor in felis consequat fermentum. Nunc neque mauris, dictum sit amet condimentum laoreet, tempus in nibh. In sed tincidunt quam. Donec ut interdum massa. Cras eu nibh efficitur, aliquam justo ac, finibus diam. Fusce at finibus ipsum. Donec sit amet nunc at tellus vehicula luctus. Quisque et lacus condimentum, semper lorem non, varius nisl. In dignissim quam ut augue congue suscipit. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras suscipit molestie consequat. Vestibulum at auctor mi, vitae pellentesque urna. Phasellus eleifend dui eget tellus feugiat dictum at sed ex integer.");
        }
    }
}
